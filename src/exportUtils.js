// ─── Export helpers: build a downloadable file from chosen sections ───

export const EXPORT_SECTIONS = [
  { id: "overview", label: "When to use & assumptions" },
  { id: "sap", label: "SAP text" },
  { id: "example", label: "Results example" },
  { id: "report", label: "How to report" },
  { id: "jasp", label: "JASP steps" },
  { id: "r", label: "R code" },
];

export const EXPORT_FORMATS = [
  { id: "txt", label: "Text (.txt)" },
  { id: "md", label: "Markdown (.md)" },
  { id: "R", label: "R script (.R)" },
];

export const DEFAULT_EXPORT_SECTIONS = ["sap", "jasp", "r"];

// Returns [{ label, body, code }] for one test/descriptive entry, in a fixed order
function sectionsFor(t, selected, useTraditional) {
  const out = [];
  const has = (id) => selected.includes(id);
  if (has("overview")) {
    const parts = [];
    if (t.when) parts.push(`When to use: ${t.when}`);
    if (t.assumptions) parts.push(`Assumptions: ${t.assumptions}`);
    if (t.summary) parts.push(`Summary measure: ${t.summary}`);
    if (t.visual) parts.push(`Suggested visual: ${t.visual}`);
    if (parts.length) out.push({ label: "Overview", body: parts.join("\n") });
  }
  if (has("sap")) {
    const sap = useTraditional ? (t.sapTraditional || t.sap) : t.sap;
    if (sap) out.push({ label: "SAP Text", body: sap });
  }
  if (has("example")) {
    const ex = useTraditional ? (t.exampleTraditional || t.example) : t.example;
    if (ex) out.push({ label: "Results Example", body: ex });
  }
  if (has("report") && t.report) out.push({ label: "How to Report", body: t.report });
  if (has("jasp") && t.jasp) out.push({ label: "JASP Steps", body: t.jasp });
  if (has("r") && t.r) out.push({ label: "R Code", body: t.r, code: true });
  return out;
}

const indent = (s, pad) => s.split("\n").map((l) => pad + l).join("\n");
const comment = (s) => s.split("\n").map((l) => (l.trim() ? `# ${l}` : "#")).join("\n");

/**
 * doc = {
 *   title: string,
 *   meta: [string],                       // lines under the title
 *   objectives: [{ heading, meta: [string], useTraditional, entries: [testObj] }],
 *   footer: [string],
 * }
 */
export function buildExport(doc, selected, format) {
  const lines = [];
  const sep = "═".repeat(50);

  if (format === "md") {
    lines.push(`# ${doc.title}`, "");
    doc.meta.forEach((m) => lines.push(`${m}  `));
    lines.push("");
    doc.objectives.forEach((o) => {
      if (o.heading) lines.push(`## ${o.heading}`, "");
      o.meta.forEach((m) => lines.push(`- ${m}`));
      if (o.meta.length) lines.push("");
      o.entries.filter(Boolean).forEach((t) => {
        lines.push(`### ${t.name}`, "");
        sectionsFor(t, selected, o.useTraditional).forEach((s) => {
          lines.push(`**${s.label}**`, "");
          if (s.code) lines.push("```r", s.body, "```", "");
          else if (s.label === "JASP Steps") lines.push("```", s.body, "```", "");
          else lines.push(s.body, "");
        });
      });
    });
    lines.push("---", ...doc.footer.map((f) => `*${f}*  `));
  } else if (format === "R") {
    lines.push(comment(`${doc.title}\n${"=".repeat(50)}`));
    doc.meta.forEach((m) => lines.push(comment(m)));
    lines.push("");
    doc.objectives.forEach((o) => {
      if (o.heading) lines.push(`# ${"─".repeat(60)}`, comment(o.heading), `# ${"─".repeat(60)}`);
      o.meta.forEach((m) => lines.push(comment(m)));
      o.entries.filter(Boolean).forEach((t) => {
        lines.push("", `# ── ${t.name} ──`);
        sectionsFor(t, selected, o.useTraditional).forEach((s) => {
          if (s.code) lines.push("", s.body);
          else lines.push("#", comment(`${s.label}:`), comment(s.body));
        });
      });
      lines.push("", "");
    });
    doc.footer.forEach((f) => lines.push(comment(f)));
  } else {
    lines.push(sep, doc.title.toUpperCase(), sep, "");
    doc.meta.forEach((m) => lines.push(m));
    if (doc.meta.length) lines.push("");
    doc.objectives.forEach((o) => {
      if (o.heading) lines.push(o.heading, "─".repeat(30));
      o.meta.forEach((m) => lines.push(m));
      lines.push("");
      o.entries.filter(Boolean).forEach((t) => {
        lines.push(`  Test/Method: ${t.name}`);
        sectionsFor(t, selected, o.useTraditional).forEach((s) => {
          lines.push(`  ${s.label}:`, indent(s.body, "    "), "");
        });
      });
      lines.push("");
    });
    lines.push("─".repeat(50), ...doc.footer);
  }
  return lines.join("\n");
}

export function downloadText(text, baseName, format) {
  const mime = format === "md" ? "text/markdown" : "text/plain";
  const blob = new Blob([text], { type: `${mime};charset=utf-8` });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = `${baseName}.${format}`;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(a.href), 1000);
}
