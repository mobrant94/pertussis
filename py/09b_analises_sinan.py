# 09b_analises_sinan.py
# Análises com os microdados dos casos confirmados em menores de 5 anos:
# estado vacinal, efetividade da vacina pelo método de rastreamento (screening method),
# tempo desde a última dose, gravidade, apresentação clínica e equidade.
# Compara 2016–2019 (referência) com julho de 2024 a dezembro de 2025 (onda).
import os, numpy as np, pandas as pd, pyreadr
import statsmodels.api as sm, statsmodels.formula.api as smf

os.chdir("/tmp/teste2")
OUT = "saidas/sinan_ind"
d = pd.read_parquet(f"{OUT}/casos_confirmados_lt5.parquet")
P = ["2016–2019", "jul/2024–dez/2025"]
d = d[d["periodo"].isin(P)].copy()
d["periodo"] = pd.Categorical(d["periodo"], P)
FX = ["< 2 meses", "2–3 meses", "4–5 meses", "6–11 meses", "1–4 anos"]
REG = ["Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"]
res = {}

def pct(x): return 100 * x.mean()

# ---------------------------------------------------------------- 1. estado vacinal
SIT = ["Sem idade para vacinar", "Nunca vacinado", "Atrasado para a idade", "Em dia para a idade", "Ignorado"]
t1 = (d.groupby(["faixa", "periodo"], observed=True)["situacao"].value_counts(normalize=True).mul(100)
        .unstack().reindex(columns=SIT).fillna(0).round(1))
t1["n"] = d.groupby(["faixa", "periodo"], observed=True).size()
t1.to_csv(f"{OUT}/t1_situacao_vacinal.csv")
# entre os com informação, 6 meses a 4 anos, por região (onda)
c = d[(d["idade_meses"] >= 6) & (d["situacao"] != "Ignorado")]
t1r = (c.groupby(["periodo", "regiao"], observed=True)["situacao"].value_counts(normalize=True).mul(100)
         .unstack().reindex(columns=SIT[1:4]).fillna(0).round(1))
t1r["n"] = c.groupby(["periodo", "regiao"], observed=True).size()
t1r.to_csv(f"{OUT}/t1r_situacao_vacinal_regiao.csv")
res["ignorado"] = d.groupby("periodo", observed=True)["situacao"].apply(lambda s: pct(s == "Ignorado")).round(1)

# ---------------------------------------------------------------- 2. efetividade (screening method)
# PPV: cobertura da 3ª dose de DTP (pentavalente) da coorte de nascimento do caso, por UF,
# ponderada pelos nascidos vivos (cobertura bruta, sem limite municipal), limitada a 99%.
cob = pyreadr.read_r("dados/processados/cobertura_mun_ano.rds")[None]
cob["uf"] = cob["code_muni"].astype(str).str[:2]
cob = cob.dropna(subset=["dtp_bruta", "nv"])
cuf = (cob.assign(dose=cob["dtp_bruta"] * cob["nv"]).groupby(["uf", "ano"])[["dose", "nv"]].sum()
          .assign(ppv=lambda x: (x["dose"] / x["nv"] / 100).clip(upper=.99)).reset_index()[["uf", "ano", "ppv"]])
ult = cuf[cuf["ano"] == cuf["ano"].max()].drop(columns="ano")
e = d[(d["idade_meses"] >= 7) & d["n_doses"].notna()].copy()
e["vac3"] = (e["n_doses"] >= 3).astype(int)
e["ano_c"] = e["ano_nasc"].clip(upper=int(cuf["ano"].max()))
e = e.merge(cuf.rename(columns={"ano": "ano_c"}), on=["uf", "ano_c"], how="left")
e["off"] = np.log(e["ppv"] / (1 - e["ppv"]))
e["grupo"] = np.where(e["idade_meses"] < 12, "7–11 meses", "1–4 anos")

def ve(sub):
    sub = sub.dropna(subset=["off"])
    if len(sub) < 30 or sub["vac3"].nunique() < 2: return None
    m = smf.glm("vac3 ~ 1", data=sub, family=sm.families.Binomial(), offset=sub["off"]).fit()
    b, se = m.params.iloc[0], m.bse.iloc[0]
    return dict(n=len(sub), pcv=100 * sub["vac3"].mean(), ppv=100 * sub["ppv"].mean(),
                ve=100 * (1 - np.exp(b)), lo=100 * (1 - np.exp(b + 1.96 * se)), hi=100 * (1 - np.exp(b - 1.96 * se)))
rows = []
for per in P:
    for g in ["7–11 meses", "1–4 anos", "Total"]:
        s = e[(e["periodo"] == per) & ((e["grupo"] == g) | (g == "Total"))]
        r = ve(s)
        if r: rows.append(dict(periodo=per, grupo=g, regiao="Brasil", **r))
        if g == "Total":
            for rg in REG:
                r = ve(s[s["regiao"] == rg])
                if r: rows.append(dict(periodo=per, grupo=g, regiao=rg, **r))
t2 = pd.DataFrame(rows).round(1); t2.to_csv(f"{OUT}/t2_efetividade_screening.csv", index=False)
# por idade em anos na onda (1, 2, 3 e 4 anos): queda da proteção com a idade
rows = []
for per in P:
    for a, (lo_, hi_) in {"7–11 meses": (7, 12), "1 ano": (12, 24), "2 anos": (24, 36), "3 anos": (36, 48), "4 anos": (48, 60)}.items():
        r = ve(e[(e["periodo"] == per) & e["idade_meses"].between(lo_, hi_ - .001)])
        if r: rows.append(dict(periodo=per, idade=a, **r))
t2a = pd.DataFrame(rows).round(1); t2a.to_csv(f"{OUT}/t2a_efetividade_idade.csv", index=False)

# ---------------------------------------------------------------- 3. tempo desde a última dose
u = d[(d["idade_meses"] >= 12) & (d["n_doses"] >= 3) & d["dias_ult_dose"].notna()].copy()
u["anos"] = u["dias_ult_dose"] / 365.25
t3 = u.groupby("periodo", observed=True)["anos"].describe(percentiles=[.25, .5, .75]).round(2)
t3["pct_mais_1ano"] = u.groupby("periodo", observed=True)["anos"].apply(lambda x: pct(x > 1)).round(1)
t3["pct_mais_2anos"] = u.groupby("periodo", observed=True)["anos"].apply(lambda x: pct(x > 2)).round(1)
t3["pct_reforco"] = u.groupby("periodo", observed=True)["n_doses"].apply(lambda x: pct(x >= 4)).round(1)
t3.to_csv(f"{OUT}/t3_tempo_ultima_dose.csv")
u.to_parquet(f"{OUT}/tempo_ultima_dose.parquet", index=False)

# ---------------------------------------------------------------- 4. gravidade
G = {"cs_hospita": "Internação", "cs_pneumon": "Pneumonia", "cs_encefal": "Encefalopatia", "cs_desitra": "Desidratação", "obito": "Óbito"}
def taxa(s, v): x = s[v].dropna(); return pd.Series({"pct": pct(x), "n": int(x.sum()), "N": len(x)})
t4 = pd.concat({lab: d.groupby(["faixa", "periodo"], observed=True).apply(lambda s: taxa(s, v)) for v, lab in G.items()}, axis=1)
t4.to_csv(f"{OUT}/t4_gravidade_faixa.csv")
t4r = pd.concat({lab: d.groupby(["periodo", "regiao"], observed=True).apply(lambda s: taxa(s, v)) for v, lab in G.items()}, axis=1)
t4r.to_csv(f"{OUT}/t4r_gravidade_regiao.csv")
res["obitos"] = d.groupby(["periodo", "faixa"], observed=True)["obito"].sum()

def rp(df, y, x, adj):
    """Razão de prevalência por regressão de Poisson com variância robusta."""
    df = df.dropna(subset=[y]).copy()
    for c_ in df.columns:                      # categorias sem casos geram colunas vazias
        if isinstance(df[c_].dtype, pd.CategoricalDtype): df[c_] = df[c_].astype(str)
    try:
        m = smf.glm(f"{y} ~ {x} + {adj}", data=df, family=sm.families.Poisson()).fit(cov_type="HC0")
    except Exception as err:
        print("modelo não estimado:", y, x, err); return {}
    k = [i for i in m.params.index if i.startswith(x)]
    return {i: (np.exp(m.params[i]), np.exp(m.conf_int().loc[i, 0]), np.exp(m.conf_int().loc[i, 1]), len(df)) for i in k}

# gravidade segundo o estado vacinal, 6 meses a 4 anos (em dia vs atrasado/nunca vacinado)
v = d[(d["idade_meses"] >= 6) & d["situacao"].isin(["Em dia para a idade", "Atrasado para a idade", "Nunca vacinado"])].copy()
v["em_dia"] = (v["situacao"] == "Em dia para a idade").astype(int)
rows = []
for y, lab in G.items():
    if y == "obito" or y == "cs_encefal": continue
    for k, (r, l, h, n) in rp(v, y, "em_dia", "C(faixa3) + C(regiao) + C(periodo)").items():
        rows.append(dict(desfecho=lab, grupo="6 meses–4 anos", comparacao="Em dia vs atrasado/nunca vacinado", rp=r, lo=l, hi=h, n=n))
# lactentes de 2 a 5 meses: ao menos uma dose vs nenhuma
i2 = d[d["idade_meses"].between(2, 5.999) & d["n_doses"].notna()].copy()
i2["vac1"] = (i2["n_doses"] >= 1).astype(int)
for y, lab in G.items():
    if y == "cs_encefal": continue
    for k, (r, l, h, n) in rp(i2, y, "vac1", "C(faixa) + C(regiao) + C(periodo)").items():
        rows.append(dict(desfecho=lab, grupo="2–5 meses", comparacao="≥ 1 dose vs nenhuma", rp=r, lo=l, hi=h, n=n))
t4v = pd.DataFrame(rows).round(2); t4v.to_csv(f"{OUT}/t4v_gravidade_vacinacao.csv", index=False)

# ---------------------------------------------------------------- 5. apresentação clínica
S = {"cs_tosse_e": "Tosse", "cs_tosse_p": "Tosse paroxística", "cs_crise": "Guincho inspiratório", "cs_cianose": "Cianose",
     "cs_vomitos": "Vômito pós-tosse", "cs_apneia": "Apneia", "cs_temp_38": "Febre ≥ 38 °C"}
t5 = pd.concat({lab: d.groupby(["faixa3", "periodo"], observed=True)[v_].apply(lambda x: pct(x.dropna())) for v_, lab in S.items()}, axis=1).round(1)
t5.to_csv(f"{OUT}/t5_sintomas_faixa.csv")
rows = []
for y, lab in S.items():
    for k, (r, l, h, n) in rp(v, y, "em_dia", "C(faixa3) + C(regiao) + C(periodo)").items():
        p1 = pct(v.loc[v["em_dia"] == 1, y].dropna()); p0 = pct(v.loc[v["em_dia"] == 0, y].dropna())
        rows.append(dict(sintoma=lab, pct_em_dia=p1, pct_atrasado=p0, rp=r, lo=l, hi=h, n=n))
t5v = pd.DataFrame(rows).round(2); t5v.to_csv(f"{OUT}/t5v_sintomas_vacinacao.csv", index=False)

# ---------------------------------------------------------------- 6. equidade
t6 = pd.concat({"raca": d.groupby("periodo", observed=True)["raca"].value_counts(normalize=True).mul(100),
                "sexo": d.groupby("periodo", observed=True)["sexo"].value_counts(normalize=True).mul(100)}).round(1)
t6.to_csv(f"{OUT}/t6_raca_sexo.csv")
q = d[d["raca"].isin(["Branca", "Preta", "Parda", "Indígena"])].copy()
q["raca"] = pd.Categorical(q["raca"], ["Branca", "Preta", "Parda", "Indígena"])
rows = []
for y, lab in {"cs_hospita": "Internação", "obito": "Óbito", "cs_pneumon": "Pneumonia"}.items():
    for k, (r, l, h, n) in rp(q, y, "C(raca)", "C(faixa) + C(regiao) + C(periodo)").items():
        g = k.split("T.")[1].rstrip("]")
        rows.append(dict(desfecho=lab, raca=g, pct=pct(q.loc[q["raca"] == g, y].dropna()), rp=r, lo=l, hi=h, n=n))
    rows.append(dict(desfecho=lab, raca="Branca (ref.)", pct=pct(q.loc[q["raca"] == "Branca", y].dropna()), rp=1, lo=np.nan, hi=np.nan, n=np.nan))
# situação vacinal por raça (6 meses–4 anos, com informação)
t6s = (v[v["raca"] != "Ignorada"].groupby("raca")["situacao"].value_counts(normalize=True).mul(100).unstack().round(1))
t6s.to_csv(f"{OUT}/t6s_vacinacao_raca.csv")
t6g = pd.DataFrame(rows).round(2)
t6g["eventos"] = t6g["raca"].map(lambda g: int(q.loc[q["raca"] == g.replace(" (ref.)", ""), "obito"].sum()))
t6g.to_csv(f"{OUT}/t6g_gravidade_raca.csv", index=False)
sx = d.dropna(subset=["sexo"]).copy(); sx["fem"] = (sx["sexo"] == "Feminino").astype(int)
rows = []
for y, lab in {"cs_hospita": "Internação", "obito": "Óbito"}.items():
    for k, (r, l, h, n) in rp(sx, y, "fem", "C(faixa) + C(regiao) + C(periodo)").items():
        rows.append(dict(desfecho=lab, rp=r, lo=l, hi=h, n=n))
pd.DataFrame(rows).round(2).to_csv(f"{OUT}/t6x_gravidade_sexo.csv", index=False)
tot = d.groupby("periodo", observed=True).agg(casos=("obito", "size"), obitos=("obito", "sum"), letal=("obito", "mean"),
                                         hosp=("cs_hospita", "mean"), lab=("laboratorio", "mean"))
tot.to_csv(f"{OUT}/t0_totais.csv"); print(tot)
print("ok")

# ---------------------------------------------------------------- 2b. sensibilidade da efetividade
# (a) casos com estado vacinal ignorado tratados como não vacinados (limite superior da efetividade);
# (b) PPV com a cobertura da coorte acrescida de 5 pontos percentuais (subregistro de doses no SI-PNI).
e2 = d[d["idade_meses"] >= 7].copy()
e2["vac3"] = (e2["n_doses"].fillna(0) >= 3).astype(int)
e2["ano_c"] = e2["ano_nasc"].clip(upper=int(cuf["ano"].max()))
e2 = e2.merge(cuf.rename(columns={"ano": "ano_c"}), on=["uf", "ano_c"], how="left")
e2["off"] = np.log(e2["ppv"] / (1 - e2["ppv"]))
e3 = e.copy(); e3["ppv"] = (e3["ppv"] + .05).clip(upper=.99); e3["off"] = np.log(e3["ppv"] / (1 - e3["ppv"]))
rows = []
for per in P:
    for a, (lo_, hi_) in {"7–11 meses": (7, 12), "1 ano": (12, 24), "2–4 anos": (24, 60)}.items():
        for nome, base in {"Principal": e, "Ignorado = não vacinado": e2, "Cobertura + 5 p.p.": e3}.items():
            r = ve(base[(base["periodo"] == per) & base["idade_meses"].between(lo_, hi_ - .001)])
            if r: rows.append(dict(periodo=per, idade=a, cenario=nome, **r))
pd.DataFrame(rows).round(1).to_csv(f"{OUT}/t2s_efetividade_sensibilidade.csv", index=False)
