# 09d_relatorio.py — relatório das análises com os microdados do SINAN (Markdown -> Word)
import os, subprocess, pandas as pd, numpy as np
os.chdir("/tmp/teste2"); O = "saidas/sinan_ind"
f = lambda x, d=1: (f"{x:,.{d}f}").replace(",", "X").replace(".", ",").replace("X", ".").replace("-", "−")
P = ["2016–2019", "jul/2024–dez/2025"]
t0 = pd.read_csv(f"{O}/t0_totais.csv", index_col=0)
t1 = pd.read_csv(f"{O}/t1_situacao_vacinal.csv")
t1r = pd.read_csv(f"{O}/t1r_situacao_vacinal_regiao.csv")
t2 = pd.read_csv(f"{O}/t2_efetividade_screening.csv")
t2a = pd.read_csv(f"{O}/t2a_efetividade_idade.csv")
t2s = pd.read_csv(f"{O}/t2s_efetividade_sensibilidade.csv")
t3 = pd.read_csv(f"{O}/t3_tempo_ultima_dose.csv", index_col=0)
t4 = pd.read_csv(f"{O}/t4_gravidade_faixa.csv", header=[0, 1], index_col=[0, 1])
t4r = pd.read_csv(f"{O}/t4r_gravidade_regiao.csv", header=[0, 1], index_col=[0, 1])
t4v = pd.read_csv(f"{O}/t4v_gravidade_vacinacao.csv")
t5 = pd.read_csv(f"{O}/t5_sintomas_faixa.csv")
t5v = pd.read_csv(f"{O}/t5v_sintomas_vacinacao.csv")
t6 = pd.read_csv(f"{O}/t6_raca_sexo.csv")
t6g = pd.read_csv(f"{O}/t6g_gravidade_raca.csv")
t6s = pd.read_csv(f"{O}/t6s_vacinacao_raca.csv", index_col=0)
t6x = pd.read_csv(f"{O}/t6x_gravidade_sexo.csv")
ic = lambda r, d=2: f"{f(r['rp'], d)} ({f(r['lo'], d)}–{f(r['hi'], d)})"
icv = lambda r: f"{f(r['ve'], 0)}% ({f(r['lo'], 0)} a {f(r['hi'], 0)})"
FX = ["< 2 meses", "2–3 meses", "4–5 meses", "6–11 meses", "1–4 anos"]
def md(df): return df.to_markdown(index=False)
A = lambda per, idade: t2a[(t2a.periodo == per) & (t2a.idade == idade)].iloc[0]
S = lambda per, idade, cen: t2s[(t2s.periodo == per) & (t2s.idade == idade) & (t2s.cenario == cen)].iloc[0]
G = lambda per, fx, v: t4.loc[(fx, per), (v, "pct")]
GN = lambda per, fx, v: int(t4.loc[(fx, per), (v, "n")])
V = lambda des, gr: t4v[(t4v.desfecho == des) & (t4v.grupo == gr)].iloc[0]
s1 = lambda fx, per, c: t1[(t1.faixa == fx) & (t1.periodo == per)][c].iloc[0]

L = []
L += ["# Coqueluche em menores de 5 anos: estado vacinal, efetividade da vacina, gravidade, quadro clínico e equidade nos casos notificados ao SINAN", "",
      "Análises complementares com os microdados dos casos confirmados (SINAN), com os mesmos critérios de inclusão do artigo, comparando 2016–2019 com julho de 2024 a dezembro de 2025.", ""]
L += ["## Métodos", "",
      f"Foram analisados os casos confirmados em menores de 5 anos residentes nos municípios do painel do artigo: {f(t0.loc['2016–2019','casos'],0)} em 2016–2019 e {f(t0.loc['jul/2024–dez/2025','casos'],0)} de julho de 2024 a dezembro de 2025. O número de doses de vacina contra a coqueluche (pentavalente ou DTP) registrado na ficha foi comparado com o esperado para a idade pelo calendário (2, 4 e 6 meses, com reforços aos 15 meses e aos 4 anos), classificando cada caso como sem idade para vacinar, nunca vacinado, atrasado ou em dia. A efetividade da vacina foi estimada pelo método de rastreamento (*screening method*), que compara a proporção de casos com três ou mais doses com a cobertura da terceira dose na população; a cobertura foi a da unidade da federação e do ano de nascimento de cada caso, ponderada pelos nascidos vivos, e o modelo foi uma regressão logística com o logito da cobertura como *offset*. Casos com 1 ou 2 doses foram classificados como não completamente vacinados, e casos com estado vacinal ignorado foram excluídos na análise principal. Gravidade, quadro clínico e equidade foram descritos por faixa etária, período e região; as comparações entre grupos usaram razões de prevalência (RP) estimadas por regressão de Poisson com variância robusta, ajustadas por faixa etária, região e período.", ""]

# 1. estado vacinal
L += ["## 1. Estado vacinal dos casos", "",
      f"De 6 meses a 4 anos, a maioria dos casos ocorreu em crianças com o esquema em dia para a idade: na onda, {f(s1('6–11 meses',P[1],'Em dia para a idade'))}% dos casos de 6 a 11 meses e {f(s1('1–4 anos',P[1],'Em dia para a idade'))}% dos de 1 a 4 anos, contra {f(s1('6–11 meses',P[0],'Em dia para a idade'))}% e {f(s1('1–4 anos',P[0],'Em dia para a idade'))}% em 2016–2019. Crianças nunca vacinadas foram {f(s1('6–11 meses',P[1],'Nunca vacinado'))}% e {f(s1('1–4 anos',P[1],'Nunca vacinado'))}% dos casos nessas faixas. Abaixo de 2 meses, {f(s1('< 2 meses',P[1],'Sem idade para vacinar'))}% dos casos ainda não tinham idade para a primeira dose. A informação foi ignorada ou estava em branco em 17% a 23% dos casos na onda e em 19% a 27% em 2016–2019 (Tabela 1, Figura A).", ""]
tb = t1[t1.periodo.isin(P)].copy()
tb["faixa"] = pd.Categorical(tb["faixa"], FX); tb = tb.sort_values(["faixa", "periodo"])
tb = tb.rename(columns={"faixa": "Faixa etária", "periodo": "Período", "n": "Casos"})
for c in ["Sem idade para vacinar", "Nunca vacinado", "Atrasado para a idade", "Em dia para a idade", "Ignorado"]: tb[c] = tb[c].map(f)
tb["Casos"] = tb["Casos"].map(lambda x: f(x, 0))
L += ["**Tabela 1.** Situação vacinal dos casos confirmados por faixa etária e período (% dos casos).", "", md(tb), ""]
rr = t1r[t1r.periodo == P[1]]
L += [f"Na onda, entre os casos de 6 meses a 4 anos com informação, a proporção de nunca vacinados foi maior no Norte ({f(rr[rr.regiao=='Norte']['Nunca vacinado'].iloc[0])}%), no Centro-Oeste ({f(rr[rr.regiao=='Centro-Oeste']['Nunca vacinado'].iloc[0])}%) e no Nordeste ({f(rr[rr.regiao=='Nordeste']['Nunca vacinado'].iloc[0])}%) do que no Sudeste ({f(rr[rr.regiao=='Sudeste']['Nunca vacinado'].iloc[0])}%) e no Sul ({f(rr[rr.regiao=='Sul']['Nunca vacinado'].iloc[0])}%), regiões que concentraram os casos e nas quais cerca de quatro em cada cinco casos estavam com o esquema em dia.", ""]
L += ["![Figura A. Situação vacinal dos casos confirmados por faixa etária, 2016–2019 e julho de 2024 a dezembro de 2025.](saidas/sinan_ind/figB_situacao_vacinal.png)", ""]

# 2. efetividade
a711, a1, a2, a3, a4 = [A(P[1], i) for i in ["7–11 meses", "1 ano", "2 anos", "3 anos", "4 anos"]]
b711, b1, b2 = [A(P[0], i) for i in ["7–11 meses", "1 ano", "2 anos"]]
s24 = [S(P[1], "2–4 anos", c) for c in ["Principal", "Ignorado = não vacinado", "Cobertura + 5 p.p."]]
s24r = S(P[0], "2–4 anos", "Principal")
L += ["## 2. Efetividade da vacina (método de rastreamento)", "",
      f"Em 2016–2019, a efetividade estimada de três ou mais doses foi de {icv(b711)} aos 7–11 meses, {icv(b1)} com 1 ano e {icv(s24r)} de 2 a 4 anos. Na onda, manteve-se alta aos 7–11 meses ({icv(a711)}) e com 1 ano ({icv(a1)}), mas caiu de 2 a 4 anos: {icv(a2)} aos 2 anos, {icv(a3)} aos 3 anos e {icv(a4)} aos 4 anos (Tabela 2, Figura B). A queda de 2 a 4 anos persistiu nas análises de sensibilidade, com efetividade de {f(s24[1]['ve'],0)}% quando os casos com estado vacinal ignorado foram tratados como não vacinados e de {f(s24[2]['ve'],0)}% com a cobertura acrescida de 5 pontos percentuais, contra {f(s24[0]['ve'],0)}% na análise principal.", "",
      "Essas crianças nasceram entre 2019 e 2023 e, portanto, receberam a série primária durante a pandemia, quando a circulação da bactéria foi mínima e não houve reforço natural da imunidade. A estimativa nessas coortes, porém, depende muito da cobertura administrativa, cuja queda em 2019–2022 coincidiu com a mudança do sistema de informação do Programa Nacional de Imunizações e pode ter sido em parte subregistro de doses; uma cobertura subestimada reduz artificialmente a efetividade calculada. O resultado é compatível com perda de proteção nas coortes vacinadas durante a pandemia, mas não permite separar esse efeito do erro na cobertura.", ""]
tb = t2a.copy()
tb["Efetividade, % (IC95%)"] = tb.apply(icv, axis=1)
tb = tb.rename(columns={"periodo": "Período", "idade": "Idade", "n": "Casos com informação", "pcv": "Casos com ≥ 3 doses (%)", "ppv": "Cobertura da coorte (%)"})
tb["Casos com ≥ 3 doses (%)"] = tb["Casos com ≥ 3 doses (%)"].map(f); tb["Cobertura da coorte (%)"] = tb["Cobertura da coorte (%)"].map(f)
L += ["**Tabela 2.** Efetividade estimada de três ou mais doses de vacina contra a coqueluche, por idade e período (método de rastreamento).", "",
      md(tb[["Período", "Idade", "Casos com informação", "Casos com ≥ 3 doses (%)", "Cobertura da coorte (%)", "Efetividade, % (IC95%)"]]), ""]
rg = t2[(t2.grupo == "Total") & (t2.regiao != "Brasil") & (t2.periodo == P[1])]
L += [f"Por região, na onda, a efetividade em todas as idades de 7 a 59 meses foi menor no Sudeste ({icv(rg[rg.regiao=='Sudeste'].iloc[0])}) do que no Sul ({icv(rg[rg.regiao=='Sul'].iloc[0])}), no Nordeste ({icv(rg[rg.regiao=='Nordeste'].iloc[0])}) e no Norte ({icv(rg[rg.regiao=='Norte'].iloc[0])}).", ""]
L += ["![Figura B. Efetividade estimada de três ou mais doses de vacina contra a coqueluche por idade do caso, 2016–2019 e julho de 2024 a dezembro de 2025. Barras: intervalos de confiança de 95%.](saidas/sinan_ind/figA_efetividade_idade.png)", ""]

# 3. tempo desde a última dose
L += ["## 3. Tempo desde a última dose", "",
      f"Entre os casos de 1 a 4 anos com três ou mais doses e data da última dose registrada, a mediana do tempo entre a última dose e o início dos sintomas foi de {f(t3.loc[P[1],'50%'],2)} ano na onda (intervalo interquartil {f(t3.loc[P[1],'25%'],2)}–{f(t3.loc[P[1],'75%'],2)}), semelhante à de 2016–2019 ({f(t3.loc[P[0],'50%'],2)}; {f(t3.loc[P[0],'25%'],2)}–{f(t3.loc[P[0],'75%'],2)}); {f(t3.loc[P[1],'pct_mais_1ano'])}% adoeceram mais de um ano depois da última dose ({f(t3.loc[P[0],'pct_mais_1ano'])}% em 2016–2019). Sem a distribuição do tempo desde a última dose na população, esse dado descreve os casos, mas não mede a queda da proteção.", ""]

# 4. gravidade
L += ["## 4. Gravidade", "",
      f"A letalidade em menores de 5 anos foi de {f(100*t0.loc[P[1],'letal'],1)}% na onda ({f(t0.loc[P[1],'obitos'],0)} óbitos) e de {f(100*t0.loc[P[0],'letal'],1)}% em 2016–2019 ({f(t0.loc[P[0],'obitos'],0)} óbitos em quatro anos). Os óbitos concentraram-se nos menores de 4 meses, nos quais a letalidade aumentou: de {f(G(P[0],'< 2 meses','Óbito'))}% para {f(G(P[1],'< 2 meses','Óbito'))}% abaixo de 2 meses e de {f(G(P[0],'2–3 meses','Óbito'))}% para {f(G(P[1],'2–3 meses','Óbito'))}% aos 2–3 meses. A proporção de internados caiu em todas as faixas, sobretudo nas crianças maiores (de {f(G(P[0],'1–4 anos','Internação'))}% para {f(G(P[1],'1–4 anos','Internação'))}% de 1 a 4 anos), o que é compatível com a detecção de casos mais leves pela ampliação do PCR (Tabela 3, Figura C).", ""]
rows = []
for fx in FX:
    for per in P:
        rows.append({"Faixa etária": fx, "Período": per, "Internação (%)": f(G(per, fx, "Internação")), "Pneumonia (%)": f(G(per, fx, "Pneumonia")),
                     "Encefalopatia (%)": f(G(per, fx, "Encefalopatia")), "Óbitos": GN(per, fx, "Óbito"), "Letalidade (%)": f(G(per, fx, "Óbito"))})
L += ["**Tabela 3.** Gravidade dos casos confirmados por faixa etária e período.", "", md(pd.DataFrame(rows)), ""]
L += [f"Os casos com o esquema em dia tiveram doença menos grave. De 6 meses a 4 anos, comparados aos atrasados ou nunca vacinados, tiveram menos internação (RP {ic(V('Internação','6 meses–4 anos'))}) e pneumonia (RP {ic(V('Pneumonia','6 meses–4 anos'))}). Entre 2 e 5 meses, lactentes com ao menos uma dose tiveram menos internação (RP {ic(V('Internação','2–5 meses'))}), pneumonia (RP {ic(V('Pneumonia','2–5 meses'))}) e óbito (RP {ic(V('Óbito','2–5 meses'))}) do que os não vacinados.", ""]
rr = lambda per, rg, v: t4r.loc[(per, rg), (v, "pct")]
L += [f"Por região, a proporção de internados na onda foi maior no Norte ({f(rr(P[1],'Norte','Internação'))}%), no Nordeste ({f(rr(P[1],'Nordeste','Internação'))}%) e no Centro-Oeste ({f(rr(P[1],'Centro-Oeste','Internação'))}%) do que no Sudeste ({f(rr(P[1],'Sudeste','Internação'))}%) e no Sul ({f(rr(P[1],'Sul','Internação'))}%), onde caiu em relação a 2016–2019 ({f(rr(P[0],'Sudeste','Internação'))}% e {f(rr(P[0],'Sul','Internação'))}%). Nas regiões em que a notificação pouco aumentou, os casos registrados continuaram a ser predominantemente os internados, o que reforça a hipótese de menor detecção dos casos leves no Norte e no Nordeste.", ""]
L += ["![Figura C. Proporção de internados (A) e letalidade (B) dos casos confirmados por faixa etária, 2016–2019 e julho de 2024 a dezembro de 2025.](saidas/sinan_ind/figC_gravidade.png)", ""]

# 5. quadro clínico
q = lambda fx, per, c: t5[(t5.faixa3 == fx) & (t5.periodo == per)][c].iloc[0]
cv = t5v.set_index("sintoma")
L += ["## 5. Apresentação clínica", "",
      f"Quase todos os casos tinham tosse, mas a frequência dos sinais clássicos diminuiu na onda em todas as faixas. De 1 a 4 anos, tosse paroxística esteve presente em {f(q('1–4 anos',P[1],'Tosse paroxística'))}% dos casos (contra {f(q('1–4 anos',P[0],'Tosse paroxística'))}% em 2016–2019), guincho inspiratório em {f(q('1–4 anos',P[1],'Guincho inspiratório'))}% ({f(q('1–4 anos',P[0],'Guincho inspiratório'))}%) e cianose em {f(q('1–4 anos',P[1],'Cianose'))}% ({f(q('1–4 anos',P[0],'Cianose'))}%). Nos menores de 6 meses, apneia ocorreu em {f(q('< 6 meses',P[1],'Apneia'))}% e cianose em {f(q('< 6 meses',P[1],'Cianose'))}% dos casos. O quadro foi semelhante entre vacinados em dia e atrasados, com menos cianose nos vacinados (RP {ic(cv.loc['Cianose'])}) (Tabela 4). O quadro menos típico é compatível com a confirmação de casos mais leves pelo PCR.", ""]
rows = []
for fx in ["< 6 meses", "6–11 meses", "1–4 anos"]:
    for per in P:
        r = {"Faixa etária": fx, "Período": per}
        for c in ["Tosse paroxística", "Guincho inspiratório", "Cianose", "Vômito pós-tosse", "Apneia", "Febre ≥ 38 °C"]: r[c] = f(q(fx, per, c))
        rows.append(r)
L += ["**Tabela 4.** Sinais e sintomas dos casos confirmados por faixa etária e período (%).", "", md(pd.DataFrame(rows)), ""]

# 6. equidade
rc = t6[(t6.iloc[:, 0] == "raca")]
gr = t6g.set_index(["desfecho", "raca"])
L += ["## 6. Equidade", "",
      f"Na onda, {f(rc[(rc.iloc[:,1]==P[1]) & (rc.iloc[:,2]=='Branca')].iloc[0,3])}% dos casos eram de crianças brancas, {f(rc[(rc.iloc[:,1]==P[1]) & (rc.iloc[:,2]=='Parda')].iloc[0,3])}% pardas, {f(rc[(rc.iloc[:,1]==P[1]) & (rc.iloc[:,2]=='Preta')].iloc[0,3])}% pretas e {f(rc[(rc.iloc[:,1]==P[1]) & (rc.iloc[:,2]=='Indígena')].iloc[0,3])}% indígenas, com raça/cor ignorada em {f(rc[(rc.iloc[:,1]==P[1]) & (rc.iloc[:,2]=='Ignorada')].iloc[0,3])}%; a maior proporção de crianças brancas em relação a 2016–2019 acompanha a concentração da onda no Sul e no Sudeste. Sem denominadores por raça/cor, não foi possível calcular a incidência. Considerando os dois períodos e ajustando por faixa etária, região e período, crianças pretas (RP {ic(gr.loc[('Internação','Preta')])}) e pardas (RP {ic(gr.loc[('Internação','Parda')])}) foram mais internadas que as brancas, e a letalidade foi maior entre as indígenas (RP {ic(gr.loc[('Óbito','Indígena')])}), estimativa baseada em apenas 3 óbitos. Entre os casos de 6 meses a 4 anos, o esquema estava em dia em {f(t6s.loc['Branca','Em dia para a idade'])}% das crianças brancas, {f(t6s.loc['Parda','Em dia para a idade'])}% das pardas, {f(t6s.loc['Preta','Em dia para a idade'])}% das pretas e {f(t6s.loc['Indígena','Em dia para a idade'])}% das indígenas. Meninas foram {f(t6[(t6.iloc[:,0]=='sexo') & (t6.iloc[:,1]==P[1]) & (t6.iloc[:,2]=='Feminino')].iloc[0,3])}% dos casos, sem diferença relevante de gravidade entre os sexos (internação, RP {ic(t6x[t6x.desfecho=='Internação'].iloc[0])}).", ""]

# limitações e implicações
L += ["## Limitações", "",
      "O número de doses é informado na investigação e nem sempre é conferido na caderneta, e 17% a 27% dos casos não tinham a informação. O método de rastreamento depende da exatidão da cobertura administrativa, que tem erros no numerador e no denominador e pode ter sido subestimada em 2019–2022; a cobertura foi atribuída por unidade da federação, e não pelo município de residência, para reduzir a instabilidade dos pequenos municípios. A ficha não registra a vacinação materna com dTpa, o que impede avaliar a proteção dos lactentes pela vacina da gestante com esses dados. As comparações de gravidade e de quadro clínico entre vacinados e não vacinados são feitas apenas entre os casos e não medem a proteção contra a infecção.", ""]
L += ["## Implicações para o artigo", "",
      "A maioria dos casos de 6 meses a 4 anos ocorreu em crianças com o esquema em dia, o que confirma que a onda não foi explicada principalmente pela falta de vacinação e é coerente com a pequena fração de casos atribuível à lacuna vacinal estimada no artigo. A efetividade alta abaixo de 2 anos e menor de 2 a 4 anos durante a onda sustenta a hipótese de perda de proteção nas crianças vacinadas durante a pandemia, ainda que essa estimativa dependa da qualidade da cobertura administrativa. A vacinação também se associou a doença menos grave, inclusive com uma única dose nos lactentes. Um ponto merece revisão no texto do artigo: o Programa Nacional de Imunizações usa vacinas de células inteiras na série primária (pentavalente) e nos reforços (DTP), e as vacinas acelulares são restritas à dTpa da gestante e a situações especiais; a atribuição da queda de proteção às vacinas acelulares nas crianças deve ser ajustada, pois a proteção conferida pela vacina de células inteiras também diminui com o tempo, porém mais lentamente.", ""]
open(f"{O}/relatorio_sinan.md", "w", encoding="utf-8").write("\n".join(L))
subprocess.run(["pandoc", f"{O}/relatorio_sinan.md", "-o", f"{O}/analises_sinan_individual.docx", "--reference-doc=modelo_artigo.docx",
                "--resource-path=."], check=True)
subprocess.run(["python3", "py/ajustar_tabelas_docx.py", f"{O}/analises_sinan_individual.docx", f"{O}/analises_sinan_individual.docx"], check=True)
print("ok")
