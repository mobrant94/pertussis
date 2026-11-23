# 09c_figuras_sinan.py — figuras das análises com os microdados do SINAN
import os, numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
os.chdir("/tmp/teste2"); OUT = "saidas/sinan_ind"
plt.rcParams.update({"font.family": "Liberation Serif", "font.size": 10, "axes.spines.top": False, "axes.spines.right": False,
                     "axes.titleweight": "bold", "axes.titlesize": 10.5, "axes.titlelocation": "left"})
COR = {"2016–2019": "#8C8C8C", "jul/2024–dez/2025": "#B2182B"}
P = list(COR)

# Figura A: efetividade por idade e período
t = pd.read_csv(f"{OUT}/t2a_efetividade_idade.csv")
ida = ["7–11 meses", "1 ano", "2 anos", "3 anos", "4 anos"]
fig, ax = plt.subplots(figsize=(7, 3.6))
for k, per in enumerate(P):
    s = t[t["periodo"] == per].set_index("idade").reindex(ida)
    x = np.arange(len(ida)) + (k - .5) * .25
    ax.errorbar(x, s["ve"], yerr=[s["ve"] - s["lo"], s["hi"] - s["ve"]], fmt="o", color=COR[per], ms=6, capsize=3, lw=1.3, label=per)
ax.axhline(0, color="grey", lw=.8, ls="--")
ax.set_xticks(range(len(ida)), ida); ax.set_ylim(-140, 100)
ax.set_ylabel("Efetividade estimada (%)"); ax.set_xlabel("Idade do caso")
ax.legend(frameon=False, loc="lower left"); ax.grid(axis="y", color="#EEEEEE")
fig.tight_layout(); fig.savefig(f"{OUT}/figA_efetividade_idade.png", dpi=600); plt.close()

# Figura B: situação vacinal por faixa etária
t = pd.read_csv(f"{OUT}/t1_situacao_vacinal.csv")
SIT = ["Nunca vacinado", "Atrasado para a idade", "Em dia para a idade", "Sem idade para vacinar", "Ignorado"]
cores = ["#B2182B", "#F4A582", "#2166AC", "#E3E3E3", "#A6A6A6"]
FX = ["< 2 meses", "2–3 meses", "4–5 meses", "6–11 meses", "1–4 anos"]
fig, axs = plt.subplots(1, 2, figsize=(9, 3.6), sharey=True)
for ax, per in zip(axs, P):
    s = t[t["periodo"] == per].set_index("faixa").reindex(FX)
    left = np.zeros(len(FX))
    for c, col in zip(SIT, cores):
        ax.barh(FX, s[c], left=left, color=col, edgecolor="white", lw=.6, label=c); left += s[c].values
    ax.set_title(per); ax.set_xlim(0, 100); ax.invert_yaxis(); ax.set_xlabel("% dos casos confirmados")
h, l = axs[0].get_legend_handles_labels()
fig.legend(h, l, ncol=5, frameon=False, loc="lower center", fontsize=8.5)
fig.tight_layout(rect=(0, .08, 1, 1)); fig.savefig(f"{OUT}/figB_situacao_vacinal.png", dpi=600); plt.close()

# Figura C: internação e letalidade por faixa etária
t = pd.read_csv(f"{OUT}/t4_gravidade_faixa.csv", header=[0, 1], index_col=[0, 1])
fig, axs = plt.subplots(1, 2, figsize=(9, 3.4))
for ax, (g, tit) in zip(axs, [("Internação", "A. Internação (%)"), ("Óbito", "B. Letalidade (%)")]):
    for k, per in enumerate(P):
        s = t.xs(per, level=1)[g].reindex(FX)
        ax.bar(np.arange(len(FX)) + (k - .5) * .38, s["pct"], width=.36, color=COR[per], label=per)
        for i, (v, n) in enumerate(zip(s["pct"], s["n"])):
            ax.text(i + (k - .5) * .38, v, f"{v:.1f}".replace(".", ","), ha="center", va="bottom", fontsize=7.5)
    ax.set_xticks(range(len(FX)), FX, fontsize=8.5); ax.set_title(tit)
axs[0].legend(frameon=False)
fig.tight_layout(); fig.savefig(f"{OUT}/figC_gravidade.png", dpi=600); plt.close()
print("ok")
