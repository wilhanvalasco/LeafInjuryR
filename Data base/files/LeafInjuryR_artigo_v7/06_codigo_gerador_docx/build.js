// Builds the LeafInjuryR manuscript (.docx) in Portuguese or English.
// Usage: node build.js pt|en <output.docx>
const fs = require("fs");
const path = require("path");
const D = require("docx");
const {
  Document, Packer, Paragraph, TextRun, ImageRun, Table, TableRow, TableCell, WidthType,
  AlignmentType, BorderStyle, ShadingType, HeadingLevel, TabStopType, Header, PageNumber,
  Math: OMath, MathRun, MathFraction, MathSum, MathSubScript, MathSuperScript,
  MathSubSuperScript, MathRadical, MathRoundBrackets, MathSquareBrackets, PageBreak,
} = D;

const LANG = process.argv[2] || "pt";
const OUT = process.argv[3] || `manuscript_${LANG}.docx`;
const ROOT = path.resolve(__dirname, "..");
const FONT = "Times New Roman";
const TEXT_W = 9071; // 16 cm in DXA (A4, 3 cm left, 2 cm right)

// ------------------------------------------------------------------ inline text
// ^{superscript}, _{subscript} (asterisks are literal: L*, a*, b*, C*)
function runs(text, base = {}) {
  const out = [];
  const re = /(\^\{[^}]+\}|_\{[^}]+\})/g;
  let last = 0, m;
  while ((m = re.exec(text)) !== null) {
    if (m.index > last) out.push(new TextRun({ text: text.slice(last, m.index), ...base }));
    const tok = m[0];
    if (tok[0] === "*") out.push(new TextRun({ text: tok.slice(1, -1), italics: true, ...base }));
    else if (tok[0] === "^") out.push(new TextRun({ text: tok.slice(2, -1), superScript: true, ...base }));
    else out.push(new TextRun({ text: tok.slice(2, -1), subScript: true, ...base }));
    last = m.index + tok.length;
  }
  if (last < text.length) out.push(new TextRun({ text: text.slice(last), ...base }));
  return out;
}

const body = (text) => new Paragraph({
  children: runs(text), alignment: AlignmentType.JUSTIFIED,
  indent: { firstLine: 709 }, spacing: { line: 360, after: 0 },
});

// ------------------------------------------------------------------ equations
const ST = "\u2217"; // asterisk operator, used as superscript (L∗, a∗, b∗)
const r = (t) => new MathRun(t.replace(/\|/g, "\u2223"));
// "L*" style tokens -> base with superscript asterisk
const s_ = (b) => sup(r(b), r(ST));
const sub = (b, s) => new MathSubScript({ children: [].concat(b), subScript: [].concat(s) });
const sup = (b, s) => new MathSuperScript({ children: [].concat(b), superScript: [].concat(s) });
const subsup = (b, s, p) => new MathSubSuperScript({ children: [].concat(b), subScript: [].concat(s), superScript: [].concat(p) });
const frac = (n, d) => new MathFraction({ numerator: [].concat(n), denominator: [].concat(d) });
const sum = (s, p, c) => new MathSum({ children: [].concat(c), subScript: [].concat(s), superScript: [].concat(p) });
const sqrt = (c) => new MathRadical({ children: [].concat(c) });
const par = (c) => new MathRoundBrackets({ children: [].concat(c) });
const sqb = (c) => new MathSquareBrackets({ children: [].concat(c) });

const EQ = {
  1: () => [s_("L"), r(" = 116 f"), par(frac(r("Y"), sub(r("Y"), r("n")))), r(" − 16")],
  2: () => [s_("a"), r(" = 500"), sqb([r("f"), par(frac(r("X"), sub(r("X"), r("n")))), r(" − f"), par(frac(r("Y"), sub(r("Y"), r("n"))))]),
            r(",   "), s_("b"), r(" = 200"), sqb([r("f"), par(frac(r("Y"), sub(r("Y"), r("n")))), r(" − f"), par(frac(r("Z"), sub(r("Z"), r("n"))))])],
  3: () => [subsup(r("C"), r("ab"), r(ST)), r(" = "), sqrt([sup(r("a"), r(ST + "2")), r(" + "), sup(r("b"), r(ST + "2"))]),
            r(",   "), sub(r("h"), r("ab")), r(" = atan2"), par([s_("b"), r(", "), s_("a")])],
  4: () => [r("ExG = 2g − r − b,   ExR = 1.4r − g,   r = "), frac(r("R"), r("R + G + B"))],
  5: () => [sup(sub(r("σ"), r("B")), r("2")), par(r("t")), r(" = "), sub(r("ω"), r("0")), par(r("t")), sub(r("ω"), r("1")), par(r("t")),
            sup(sqb([sub(r("μ"), r("0")), par(r("t")), r(" − "), sub(r("μ"), r("1")), par(r("t"))]), r("2"))],
  6: () => [sup(r("t"), r(ST)), r(" = "), sub(r("arg max"), r("t")), r(" "), sup(sub(r("σ"), r("B")), r("2")), par(r("t")),
            r(",   η = "), frac([sup(sub(r("σ"), r("B")), r("2")), par(sup(r("t"), r(ST)))], sup(sub(r("σ"), r("T")), r("2")))],
  7: () => [sub(r("M"), r("raw")), par(r("x, y")), r(" = 1"), sqb([subsup(r("C"), r("ab"), r(ST)), r(" > "), sub(r("t"), r("C"))]),
            r(" ∨ 1"), sqb([r("100 − "), s_("L"), r(" > "), sub(r("t"), r("D"))])],
  8: () => [r("M ∘ B = "), par(r("M ⊖ B")), r(" ⊕ B")],
  9: () => [r("Δ"), sub(r("E"), r("b")), r(" = "), sqrt([sup(par([s_("L"), r(" − "), subsup(r("L"), r("b"), r(ST))]), r("2")), r(" + "),
            sup(par([s_("a"), r(" − "), subsup(r("a"), r("b"), r(ST))]), r("2")), r(" + "), sup(par([s_("b"), r(" − "), subsup(r("b"), r("b"), r(ST))]), r("2"))])],
  10: () => [r("S"), par(r("x, y")), r(" = 1  ⇔  "), sub(r("h"), r("min")), r(" ≤ "), sub(r("h"), r("ab")), r(" ≤ "), sub(r("h"), r("max")),
             r("  ∧  "), subsup(r("C"), r("ab"), r(ST)), r(" ≥ "), sub(r("C"), r("min"))],
  11: () => [sub(r("A"), r("f")), r(" = "), sum(r("x=1"), r("W"), sum(r("y=1"), r("H"), [sub(r("M"), r("f")), par(r("x, y"))]))],
  12: () => [sub(r("A"), r("i")), r(" = "), sum(r("x=1"), r("W"), sum(r("y=1"), r("H"), [sub(r("M"), r("i")), par(r("x, y"))]))],
  13: () => [r("I"), par(r("%")), r(" = "), frac(sub(r("A"), r("i")), sub(r("A"), r("f"))), r(" × 100")],
  14: () => [sub(r("A"), r("i")), r(" = "), sub(r("A"), r("c")), r(" + "), sub(r("A"), r("n")), r(" + "), sub(r("A"), r("o"))],
  15: () => [sub(r("A"), r("cm²")), r(" = "), frac(sub(r("A"), r("px")), sup(r("p"), r("2")))],
  16: () => [r("IoU = "), frac(r("TP"), r("TP + FP + FN")), r(",   Dice = "), frac(r("2TP"), r("2TP + FP + FN"))],
  17: () => [r("Se = "), frac(r("TP"), r("TP + FN")), r(",  Sp = "), frac(r("TN"), r("TN + FP")), r(",  Pr = "), frac(r("TP"), r("TP + FP")),
             r(",  F1 = "), frac(r("2 · Pr · Se"), r("Pr + Se"))],
  18: () => [r("MAE = "), frac(r("1"), r("n")), sum(r("k=1"), r("n"), [r("|"), sub(r("I"), r("k")), r(" − "), sub(r("Î"), r("k")), r("|")])],
  19: () => [sub(r("ρ"), r("c")), r(" = "), frac([r("2"), sub(r("s"), r("xy"))], [sup(sub(r("s"), r("x")), r("2")), r(" + "), sup(sub(r("s"), r("y")), r("2")),
             r(" + "), sup(par([r("x̄ − ȳ")]), r("2"))])],
  20: () => [r("LoA = "), r("d̄ ± 1.96 "), sub(r("s"), r("d"))],
  21: () => [r("y = "), sub(r("β"), r("0")), r(" + "), sub(r("β"), r("1")), r("x + ε,    y = α"), sup(r("x"), r("β")), r(" + ε")],
  22: () => [sub(r("RMSE"), r("cv")), r(" = "), sqrt([frac(r("1"), r("n")), sum(r("k=1"), r("n"), sup(par([sub(r("y"), r("k")), r(" − "), sub(r("ŷ"), r("(−k)"))]), r("2")))])],
};
const eq = (b) => new Paragraph({
  tabStops: [{ type: TabStopType.CENTER, position: Math.round(TEXT_W / 2) }, { type: TabStopType.RIGHT, position: TEXT_W }],
  spacing: { before: 200, after: 200 },
  children: [new TextRun("\t"), new OMath({ children: EQ[b.id]() }), new TextRun(`\t(${b.n})`)],
});

// ------------------------------------------------------------------ figures & tables
const L = LANG === "pt"
  ? { fig: "Figura", tab: "Tabela", src: "Fonte: elaborada pelo autor (2026).", srcT: "Fonte: elaborada pelo autor (2026).", kw: "Palavras-chave:", refs: "REFERÊNCIAS" }
  : { fig: "Figure", tab: "Table", src: "Source: the author (2026).", srcT: "Source: the author (2026).", kw: "Keywords:", refs: "REFERENCES" };

function pngSize(file) {
  const b = fs.readFileSync(file);
  return { w: b.readUInt32BE(16), h: b.readUInt32BE(20), data: b };
}
function figure(num, file, caption, source, wpx0) {
  const img = pngSize(path.join(ROOT, "figures", file));
  const wpx = wpx0 || 600, hpx = Math.round(wpx * img.h / img.w);
  return [
    new Paragraph({ keepNext: true, alignment: AlignmentType.CENTER, spacing: { before: 200, after: 80, line: 240 },
      children: [new TextRun({ text: `${L.fig} ${num} – `, bold: true, size: 20 }), ...runs(caption, { size: 20 })] }),
    new Paragraph({ keepNext: true, alignment: AlignmentType.CENTER, children: [new ImageRun({ type: "png", data: img.data,
      transformation: { width: wpx, height: hpx },
      altText: { title: `${L.fig} ${num}`, description: caption.replace(/[*_^{}]/g, ""), name: file } })] }),
    new Paragraph({ alignment: AlignmentType.LEFT, spacing: { after: 240, line: 240 },
      children: runs(source || L.src, { size: 20 }) }),
  ];
}
function table(num, caption, header, rows, widths, source) {
  const total = widths.reduce((a, b) => a + b, 0);
  const W = widths.map((w) => Math.round(w / total * TEXT_W));
  W[W.length - 1] += TEXT_W - W.reduce((a, b) => a + b, 0);
  const none = { style: BorderStyle.NONE, size: 0, color: "FFFFFF" };
  const line = { style: BorderStyle.SINGLE, size: 6, color: "000000" };
  const cell = (t, i, isHead, isLast) => new TableCell({
    width: { size: W[i], type: WidthType.DXA },
    margins: { top: 40, bottom: 40, left: 70, right: 70 },
    borders: { left: none, right: none, top: isHead ? line : none, bottom: (isHead || isLast) ? line : none },
    shading: isHead ? { type: ShadingType.CLEAR, color: "auto", fill: "F2F2F2" } : undefined,
    children: [new Paragraph({ alignment: i === 0 ? AlignmentType.LEFT : AlignmentType.CENTER, spacing: { line: 240 },
      children: runs(String(t), { size: 18, bold: isHead }) })],
  });
  return [
    new Paragraph({ keepNext: true, alignment: AlignmentType.CENTER, spacing: { before: 200, after: 80, line: 240 },
      children: [new TextRun({ text: `${L.tab} ${num} – `, bold: true, size: 20 }), ...runs(caption, { size: 20 })] }),
    new Table({ width: { size: TEXT_W, type: WidthType.DXA }, columnWidths: W,
      rows: [new TableRow({ tableHeader: true, children: header.map((h, i) => cell(h, i, true, false)) }),
             ...rows.map((rw, k) => new TableRow({ children: rw.map((c, i) => cell(c, i, false, k === rows.length - 1)) }))] }),
    new Paragraph({ spacing: { before: 60, after: 240, line: 240 }, children: runs(source || L.srcT, { size: 20 }) }),
  ];
}

// ------------------------------------------------------------------ references
const REFS = require("./refs.js");
function refABNT(x) {
  const a = x.authors.length > 3 ? `${x.authors[0]} et al.` : x.authors.join("; ");
  const ch = [new TextRun(a.endsWith(".") ? `${a} ` : `${a}. `)];
  if (x.type === "article") {
    ch.push(new TextRun(`${x.title}. `), new TextRun({ text: x.journal, bold: true }));
    let s = `, v. ${x.vol}`; if (x.issue) s += `, n. ${x.issue}`; if (x.pages) s += `, ${/^\d+$/.test(x.pages) ? "art. " : "p. "}${x.pages}`;
    s += `, ${x.year}.`; if (x.doi) s += ` DOI: ${x.doi}.`;
    ch.push(new TextRun(s));
  } else if (x.type === "book") {
    ch.push(new TextRun({ text: x.title, bold: true }), new TextRun(`${x.subtitle ? ": " + x.subtitle : ""}. ${x.edition ? x.edition + ". " : ""}${x.place}: ${x.publisher}, ${x.year}.${x.doi ? " DOI: " + x.doi + "." : ""}`));
  } else {
    ch.push(new TextRun({ text: x.title, bold: true }), new TextRun(`. ${x.note}`));
  }
  return ch;
}
function refINT(x) {
  const fmt = (s) => s.replace(/^([^,]+),/, (m, sn) => sn.charAt(0) + sn.slice(1).toLowerCase().replace(/(^|[\s-])(\p{L})/gu, (q, p, c) => p + c.toUpperCase()) + ",");
  const names = x.corporate ? [x.corporateEN || x.authors[0]] : x.authors.map(fmt);
  const a = names.length > 3 ? `${names[0]} et al.` : names.join(", ").replace(/, ([^,]+)$/, names.length > 1 ? ", & $1" : ", $1");
  const ch = [new TextRun(`${a} (${x.year}). `)];
  if (x.type === "article") {
    ch.push(new TextRun(`${x.title}. `), new TextRun({ text: x.journal, italics: true }),
      new TextRun(`, ${x.vol}${x.issue ? "(" + x.issue + ")" : ""}${x.pages ? ", " + x.pages : ""}.${x.doi ? " https://doi.org/" + x.doi : ""}`));
  } else if (x.type === "book") {
    ch.push(new TextRun({ text: x.title + (x.subtitle ? ": " + x.subtitle : ""), italics: true }),
      new TextRun(`${x.edition ? " (" + x.editionEN + ")" : ""}. ${x.publisher}.${x.doi ? " https://doi.org/" + x.doi : ""}`));
  } else {
    ch.push(new TextRun({ text: x.title, italics: true }), new TextRun(`. ${x.noteEN || x.note}`));
  }
  return ch;
}
function referenceList() {
  const txt = C.map((b) => [b.text, b.caption, b.source, (b.rows || []).flat().join(" ")].join(" ")).join(" ");
  const esc = (x) => x.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const cited = REFS.filter((x) => new RegExp(esc(x.c) + "[^()]{0,90}?" + x.year).test(txt) ||
    new RegExp(esc(x.c) + "[^;]{0,60}\\(" + x.year + "\\)").test(txt));
  console.log("references cited:", cited.length, "of", REFS.length);
  const sorted = [...cited].sort((a, b) => a.sort.localeCompare(b.sort, "pt") || a.year - b.year);
  return sorted.map((x) => new Paragraph({ alignment: AlignmentType.LEFT, spacing: { line: 240, after: 240 },
    children: (LANG === "pt" ? refABNT(x) : refINT(x)).map((t) => t) }));
}

// ------------------------------------------------------------------ content
const C = require(`./content_${LANG}.js`);
const children = [];
for (const b of C) {
  switch (b.t) {
    case "title": children.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 120, line: 276 },
      children: [new TextRun({ text: b.text, bold: true, size: 28 })] })); break;
    case "subtitle": children.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 240, line: 276 },
      children: [new TextRun({ text: b.text, bold: true, italics: true, size: 24 })] })); break;
    case "authors": children.push(new Paragraph({ alignment: AlignmentType.RIGHT, spacing: { after: 60, line: 240 },
      children: runs(b.text, { size: 22 }) })); break;
    case "note": children.push(new Paragraph({ alignment: AlignmentType.RIGHT, spacing: { after: 240, line: 240 },
      children: runs(b.text, { size: 18 }) })); break;
    case "abs": children.push(
      new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 240, after: 120 }, children: [new TextRun({ text: b.label, bold: true })] }),
      new Paragraph({ alignment: AlignmentType.JUSTIFIED, spacing: { line: 240, after: 120 }, children: runs(b.text) })); break;
    case "kw": children.push(new Paragraph({ alignment: AlignmentType.JUSTIFIED, spacing: { line: 240, after: 240 },
      children: [new TextRun({ text: b.label + " ", bold: true }), ...runs(b.text)] })); break;
    case "h1": children.push(new Paragraph({ heading: HeadingLevel.HEADING_1, keepNext: true, spacing: { before: 360, after: 240 },
      children: [new TextRun({ text: b.text, bold: true, font: FONT, size: 24, color: "000000" })] })); break;
    case "h2": children.push(new Paragraph({ heading: HeadingLevel.HEADING_2, keepNext: true, spacing: { before: 240, after: 120 },
      children: [new TextRun({ text: b.text, bold: true, font: FONT, size: 24, color: "000000" })] })); break;
    case "p": children.push(body(b.text)); break;
    case "eq": children.push(eq(b)); break;
    case "fig": children.push(...figure(b.n, b.file, b.caption, b.source, b.w)); break;
    case "table": children.push(...table(b.n, b.caption, b.header, b.rows, b.widths, b.source)); break;
    case "refs": children.push(new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 360, after: 240 },
      children: [new TextRun({ text: L.refs, bold: true, font: FONT, size: 24, color: "000000" })] }), ...referenceList()); break;
    case "break": children.push(new Paragraph({ children: [new PageBreak()] })); break;
    default: throw new Error("unknown block " + b.t);
  }
}

const doc = new Document({
  creator: "Wilhan Valasco dos Santos",
  title: C[0].text,
  styles: {
    default: { document: { run: { font: FONT, size: 24 } } },
    paragraphStyles: [
      { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: FONT, size: 24, bold: true, color: "000000" }, paragraph: { outlineLevel: 0 } },
      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: FONT, size: 24, bold: true, color: "000000" }, paragraph: { outlineLevel: 1 } },
    ],
  },
  sections: [{
    properties: { page: { size: { width: 11906, height: 16838 },
      margin: { top: 1701, left: 1701, bottom: 1134, right: 1134, header: 708 } } },
    headers: { default: new Header({ children: [new Paragraph({ alignment: AlignmentType.RIGHT,
      children: [new TextRun({ children: [PageNumber.CURRENT], size: 20 })] })] }) },
    children,
  }],
});
Packer.toBuffer(doc).then((buf) => { fs.writeFileSync(OUT, buf); console.log("wrote", OUT); });
