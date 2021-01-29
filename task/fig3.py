import os
import pandas as pd
from seaborn import regplot
from matplotlib import pyplot as plt
from src.setting import setting

PATH_TODA = setting.path.path_toda
PATH_ENTER = setting.path.path_enter
PATH_FIRST_CHOICE = setting.path.path_first_choice
PATH_RESULT_FOLDER = setting.path.path_result_folder
DIR_SAVE = os.path.join(PATH_RESULT_FOLDER, "fig", "absolute_age_score")
COLS_SUBJECT = ["shingaku", "zyuken"]
MAPPER_TITLE = setting.rename.figure_outcome_age.title
XLABEL = "Age (month of birth)"
YLABEL = "High school quality"


def main():
    df = (
        (
            pd.read_csv(PATH_ENTER, encoding="cp932")
            .rename(columns={"final_hensachi": "shingaku"})
            .pipe(lambda dfx: dfx[["mst_id", "year", "shingaku", "cramschool"]])
        ).merge(
            (
                pd.read_csv(PATH_FIRST_CHOICE, encoding="cp932")
                .rename(columns={"final_hensachi": "zyuken"})
                .pipe(lambda dfx: dfx[["mst_id", "zyuken"]])
            ), on="mst_id", how="left"
        ).merge(
            (
                pd.read_csv(PATH_TODA)
                .pipe(lambda dfx: dfx[["mst_id", "year", "grade", "school_id", "relative_age", "sex"]])
            ), on=["mst_id", "year"], how="left"
        )
    )
    df_plot = (
        df
        .assign(absolute_age=lambda dfx: dfx["relative_age"] + (dfx["grade"] + 5) * 12)
        .groupby(["grade", "absolute_age", "relative_age"])
        [COLS_SUBJECT].mean().reset_index()
    )
    # Individual Figure
    marker_list = [".", "^", "s", "*", "+", "x"]
    for i, subject in enumerate(COLS_SUBJECT):
        fig, axs = plt.subplots(1, 1, figsize=(5, 3.5))
        ax = axs
        for j, grade in enumerate(df_plot["grade"].unique().tolist()):
            df_plot_slice = df_plot.loc[(df_plot["grade"] == grade), ["absolute_age", subject]].dropna()
            if df_plot_slice.shape[0] == 0:
                continue
            regplot(
                x=df_plot_slice["absolute_age"], y=df_plot_slice[subject],
                marker=marker_list[j], order=2, ci=None,
                scatter_kws={"color": "grey"}, line_kws={"color": "#2980B9"}
            )
        [spin.set_visible(False) for spin in list(ax.spines.values())]
        ax.set_xlabel(XLABEL, fontsize=20)
        ax.set_ylabel(YLABEL, fontsize=20)
        ax.set_facecolor("none")
        ax.tick_params(labelsize=15)
        ax.set_xlim(df_plot.absolute_age.min()-1, df_plot.absolute_age.max()+1)
        ax.set_xticks(range(int(df_plot.absolute_age.min()), int(df_plot.absolute_age.max()) + 1))
        ax.set_xticklabels([3, 2, 1, 12, 11, 10, 9, 8, 7, 6, 5, 4])
        [spin.set_visible(False) for spin in list(ax.spines.values())]
        os.makedirs(DIR_SAVE, exist_ok=True)
        plt.subplots_adjust(hspace=0.5, wspace=0.5)
        filname = "fig-{0}.png".format(subject)
        plt.savefig(os.path.join(DIR_SAVE, filname), facecolor="none", bbox_inches="tight")
        plt.close()
