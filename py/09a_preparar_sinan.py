# 09a_preparar_sinan.py
# Lê os microdados do SINAN de coqueluche (COQUBR10–COQUBR25.dbc), aplica os mesmos critérios
# de inclusão do artigo (deduplicação, idade < 5 anos, caso confirmado) e grava um arquivo com
# as variáveis de vacinação, quadro clínico, gravidade e características dos casos.
import os, tempfile
import numpy as np, pandas as pd
from datasus_dbc import decompress
from dbfread import DBF

os.chdir("/tmp/teste2")
COLS = ["DT_NOTIFIC", "ID_MUNICIP", "ID_UNIDADE", "DT_SIN_PRI", "ANO_NASC", "NU_IDADE_N", "CS_SEXO", "CS_RACA",
        "ID_MN_RESI", "CLASSI_FIN", "CRITERIO", "EVOLUCAO", "CS_VAC_N", "DT_ULT_DOS",
        "CS_TOSSE_E", "CS_TOSSE_P", "CS_CRISE", "CS_CIANOSE", "CS_VOMITOS", "CS_APNEIA", "CS_TEMP37", "CS_TEMP_38",
        "CS_PNEUMON", "CS_ENCEFAL", "CS_DESITRA", "CS_OTITE", "CS_DESNUTR", "CS_HOSPITA", "CS_ANTIBIO", "DT_OBITO"]

def ler(ano):
    with tempfile.TemporaryDirectory() as tmp:
        dbf = os.path.join(tmp, "x.dbf")
        decompress(f"dados_brutos/COQUBR{ano % 100:02d}.dbc", dbf)
        t = DBF(dbf, encoding="latin-1", load=True, char_decode_errors="replace")
        d = pd.DataFrame(iter(t))
    for c in COLS:
        if c not in d: d[c] = np.nan
    return d[COLS].astype(str).replace({"None": np.nan, "nan": np.nan, "": np.nan})

d = pd.concat([ler(a) for a in range(2010, 2026)], ignore_index=True)
d = d.drop_duplicates(["DT_NOTIFIC", "ID_MUNICIP", "ID_UNIDADE", "DT_SIN_PRI", "ANO_NASC", "NU_IDADE_N", "CS_SEXO", "ID_MN_RESI"])

# idade: primeiro dígito = unidade (1 hora, 2 dia, 3 mês, 4 ano)
idade = d["NU_IDADE_N"].str.zfill(4)
un = pd.to_numeric(idade.str[0], errors="coerce"); val = pd.to_numeric(idade.str[1:], errors="coerce")
d["idade_meses"] = np.select([un.isin([1, 2]), un == 3, un == 4], [val / (24 * 30.44) * (un == 1) + val / 30.44 * (un == 2), val, val * 12], np.nan)
d["dt_sin"] = pd.to_datetime(d["DT_SIN_PRI"], errors="coerce")
d["dt_ult"] = pd.to_datetime(d["DT_ULT_DOS"], errors="coerce")
d = d[(d["idade_meses"] < 60) & d["dt_sin"].dt.year.between(2010, 2025) & (d["CLASSI_FIN"] == "1")].copy()
d["code_muni"] = d["ID_MN_RESI"].str[:6]
reg = {"1": "Norte", "2": "Nordeste", "3": "Sudeste", "4": "Sul", "5": "Centro-Oeste"}
d["regiao"] = d["code_muni"].str[0].map(reg)
d["uf"] = d["code_muni"].str[:2]
d = d[d["regiao"].notna()]
# mesmos municípios do painel do artigo (com denominador)
import pyreadr
cods = set(pyreadr.read_r("dados/processados/painel_mensal.rds")[None]["code_muni"].astype(str))
d = d[d["code_muni"].isin(cods)]

# faixas etárias e períodos
d["faixa"] = pd.cut(d["idade_meses"], [-1, 1.999, 3.999, 5.999, 11.999, 59.999],
                    labels=["< 2 meses", "2–3 meses", "4–5 meses", "6–11 meses", "1–4 anos"])
d["faixa3"] = pd.cut(d["idade_meses"], [-1, 5.999, 11.999, 59.999], labels=["< 6 meses", "6–11 meses", "1–4 anos"])
d["periodo"] = np.select([d["dt_sin"].dt.year.between(2016, 2019),
                          (d["dt_sin"] >= "2024-07-01") & (d["dt_sin"] <= "2025-12-31"),
                          d["dt_sin"].dt.year.between(2010, 2015), d["dt_sin"].dt.year.between(2020, 2023)],
                         ["2016–2019", "jul/2024–dez/2025", "2010–2015", "2020–2023"], "jan–jun/2024")

# vacinação: CS_VAC_N (1 = 1 dose; 2 = 2 doses; 3 = 3 doses; 4 = 3 doses + 1 reforço;
# 5 = 3 doses + 2 reforços; 6 = nunca vacinado; 9 = ignorado)
vac = {"1": "1 dose", "2": "2 doses", "3": "3 doses", "4": "3 doses + 1 reforço", "5": "3 doses + 2 reforços",
       "6": "Nunca vacinado", "9": "Ignorado"}
d["vac"] = d["CS_VAC_N"].map(vac).fillna("Em branco")
nd = {"1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 0}
d["n_doses"] = d["CS_VAC_N"].map(nd)
# doses esperadas para a idade (calendário: 2, 4 e 6 meses; reforços aos 15 meses e 4 anos),
# com um mês de tolerância
m = d["idade_meses"]
# idade em anos completos a partir de 12 meses: 1 ano = 3 doses esperadas; 2–4 anos = 3 doses + 1 reforço
d["esperadas"] = np.select([m < 3, m < 5, m < 7, m < 24], [0, 1, 2, 3], 4)
d["situacao"] = np.select([d["n_doses"].isna(), d["n_doses"] == 0, d["n_doses"] >= d["esperadas"]],
                          ["Ignorado", "Nunca vacinado", "Em dia para a idade"], "Atrasado para a idade")
d.loc[(d["esperadas"] == 0) & (d["n_doses"] == 0), "situacao"] = "Sem idade para vacinar"
d["dias_ult_dose"] = (d["dt_sin"] - d["dt_ult"]).dt.days
d.loc[(d["dias_ult_dose"] < 0) | (d["dias_ult_dose"] > 365 * 5) | d["n_doses"].isna() | (d["n_doses"] == 0), "dias_ult_dose"] = np.nan
d["ano_nasc"] = (d["dt_sin"] - pd.to_timedelta(d["idade_meses"] * 30.44, unit="D")).dt.year

sn = lambda s: s.map({"1": 1, "2": 0})          # 1 = sim; 2 = não; 9/branco = desconhecido
for c in ["CS_TOSSE_E", "CS_TOSSE_P", "CS_CRISE", "CS_CIANOSE", "CS_VOMITOS", "CS_APNEIA", "CS_TEMP37", "CS_TEMP_38",
          "CS_PNEUMON", "CS_ENCEFAL", "CS_DESITRA", "CS_OTITE", "CS_DESNUTR", "CS_HOSPITA", "CS_ANTIBIO"]:
    d[c.lower()] = sn(d[c])
d["obito"] = d["EVOLUCAO"].map({"1": 0, "2": 1, "3": 0})          # 2 = óbito por coqueluche
d["raca"] = d["CS_RACA"].map({"1": "Branca", "2": "Preta", "3": "Amarela", "4": "Parda", "5": "Indígena"}).fillna("Ignorada")
d["sexo"] = d["CS_SEXO"].map({"M": "Masculino", "F": "Feminino"})
d["laboratorio"] = (d["CRITERIO"] == "1").astype(int)
d.to_parquet("saidas/sinan_ind/casos_confirmados_lt5.parquet", index=False)
print(len(d)); print(d["periodo"].value_counts())
