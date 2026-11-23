# Ajusta as tabelas de um .docx gerado pelo pandoc: largura total da página, colunas proporcionais
# ao conteúdo, fonte Times New Roman 9 pt e estilo de três linhas.
import re, sys, zipfile, shutil, os
src, dst = sys.argv[1], sys.argv[2]
z = zipfile.ZipFile(src); files = {n: z.read(n) for n in z.namelist()}
doc = files["word/document.xml"].decode("utf-8")
TOTAL = 9360
def fix(m):
    t = m.group(0)
    rows = re.findall(r"<w:tr[ >].*?</w:tr>", t, flags=re.S)
    ncol = len(re.findall(r"<w:gridCol\b", t))
    lens = [9] * ncol
    for r in rows:
        cells = re.findall(r"<w:tc>.*?</w:tc>", r, flags=re.S)
        for j, c in enumerate(cells[:ncol]):
            txt = "".join(re.findall(r"<w:t[^>]*>([^<]*)", c))
            longest = max([len(w) for w in txt.split()] + [0])
            lens[j] = max(lens[j], min(len(txt), 16), longest + 3)
    w = [int(TOTAL * l / sum(lens)) for l in lens]
    t = re.sub(r"<w:tblGrid>.*?</w:tblGrid>", "<w:tblGrid>" + "".join(f'<w:gridCol w:w="{x}"/>' for x in w) + "</w:tblGrid>", t, flags=re.S)
    t = re.sub(r"<w:tblW [^>]*/>", f'<w:tblW w:w="{TOTAL}" w:type="dxa"/>', t)
    t = re.sub(r"<w:tblLayout [^>]*/>", "", t)
    t = t.replace("<w:tblPr>", '<w:tblPr><w:tblLayout w:type="fixed"/>', 1)
    def cell(mm, k=[0]): return mm.group(0)
    out = []
    for r in rows:
        cells = re.findall(r"<w:tc>.*?</w:tc>", r, flags=re.S); r2 = r
        for j, c in enumerate(cells[:ncol]):
            c2 = re.sub(r"<w:tcW [^>]*/>", "", c)
            c2 = c2.replace("<w:tc>", f'<w:tc><w:tcPr><w:tcW w:w="{w[j]}" w:type="dxa"/></w:tcPr>', 1) if "<w:tcPr>" not in c2 else c2.replace("<w:tcPr>", f'<w:tcPr><w:tcW w:w="{w[j]}" w:type="dxa"/>', 1)
            r2 = r2.replace(c, c2, 1)
        t = t.replace(r, r2, 1)
    # fonte 9 pt Times New Roman em todos os runs da tabela
    t = re.sub(r"<w:r>(?!<w:rPr>)", '<w:r><w:rPr><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman"/><w:sz w:val="18"/></w:rPr>', t)
    t = re.sub(r"<w:rPr>(?!<w:rFonts)", '<w:rPr><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman"/><w:sz w:val="18"/>', t)
    return t
doc = re.sub(r"<w:tbl>.*?</w:tbl>", fix, doc, flags=re.S)
files["word/document.xml"] = doc.encode("utf-8")
with zipfile.ZipFile(dst, "w", zipfile.ZIP_DEFLATED) as o:
    for n, b in files.items(): o.writestr(n, b)
