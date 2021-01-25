from matplotlib import pyplot as plt
from seaborn import regplot
import pandas as pd
import os
import re
from src.lib.mapper import TodashiMapper, MapperProvider


dir_save = os.path.join('notebooks/Yamaguchi/RAE/result/top/fig', 'absolute_age_score')
fetch_col = ["shingaku", "zyuken"]
mapper = MapperProvider([TodashiMapper()]).provide()

def find_matching_regexen(word, dicts=dict):
    return [description for regex, description in dicts.items() if re.compile(regex).match(word)]


df = (
    (
        pd.read_csv('./data/other/todashi_shingaku/student_enter.csv', encoding='cp932')
        .rename(columns={"final_hensachi": "shingaku"})
        .pipe(lambda dfx: dfx[["mst_id", "year", "shingaku", "cramschool"]])
    ).merge(
        (
            pd.read_csv('./data/other/todashi_shingaku/student_first_choice.csv', encoding='cp932')
            .rename(columns={"final_hensachi": "zyuken"})
            .pipe(lambda dfx: dfx[["mst_id", "zyuken"]])
        ), on="mst_id", how="left"
    ).merge(
        (
            pd.read_csv('./notebooks/Yamaguchi/RAE/data/dataset1_toda.csv')
            .pipe(lambda dfx: dfx[["mst_id", "year", "grade", "school_id", "relative_age", "sex"]])
        ), on=["mst_id", "year"], how="left"
    )
)
df_plot = (
    df
    .assign(absolute_age=lambda dfx: dfx['relative_age'] + (dfx['grade'] + 5) * 12)
    # .pipe(
    #     lambda dfx: dfx.loc[dfx['year'] == 2018, :]
    # )
    .groupby(['grade', 'absolute_age', "relative_age"])
    [fetch_col].mean().reset_index()
)
# Individual Figure
marker_list = ['.', '^', 's', '*', '+', 'x']
for i, subject in enumerate(fetch_col):
    fig, axs = plt.subplots(1, 1, figsize=(5, 3.5))
    ax = axs
    for j, grade in enumerate(df_plot['grade'].unique().tolist()):
        df_plot_slice = df_plot.loc[(df_plot['grade'] == grade), ['absolute_age', subject]].dropna()
        if df_plot_slice.shape[0] == 0:
            continue
        # ax.scatter(
        #     df_plot_slice['absolute_age'],
        #     df_plot_slice[subject],
        #     # label='Grade{i}'.format(i=int(grade)),
        #     marker=marker_list[j]
        # )
        regplot(
            x=df_plot_slice['absolute_age'], y=df_plot_slice[subject], 
            marker=marker_list[j], order=2, ci=None,
            scatter_kws={"color": "grey", }, line_kws={"color": "#2980B9", }
        )
    [spin.set_visible(False) for spin in list(ax.spines.values())]
    ax.set_xlabel('Age (month of birth)', fontsize=20)
    ax.set_ylabel('High school quality', fontsize=20)
    # titles = find_matching_regexen(subject, mapper)
    # if len(titles) != 0:
    #     print(titles)
    # ax.set_title(titles[0], fontsize=20)
    ax.set_facecolor('none')
    # ax.legend(frameon=False, fontsize=15, loc="upper left", bbox_to_anchor=(1, 1))
    ax.tick_params(labelsize=15)
    ax.set_xlim(df_plot.absolute_age.min()-1, df_plot.absolute_age.max()+1)
    ax.set_xticks( range(int(df_plot.absolute_age.min()), int(df_plot.absolute_age.max()) + 1) )
    ax.set_xticklabels([3, 2, 1, 12, 11, 10, 9, 8, 7, 6, 5, 4])
    if subject in ['gakuryoku', 'kokugo_level', 'math_level', 'eng_level']:
        ax.set_ylim(-1.5, 1.5)
    [spin.set_visible(False) for spin in list(ax.spines.values())]
    os.makedirs(dir_save, exist_ok=True)
    plt.subplots_adjust(hspace=0.5, wspace=0.5)
    filname = 'fig-{0}.png'.format(subject)
    plt.savefig(os.path.join(dir_save, filname), facecolor='none', bbox_inches="tight")
    filname = 'fig-{0}.pdf'.format(subject)
    plt.savefig(os.path.join(dir_save, filname), facecolor='none', bbox_inches="tight")
    # plt.show()
    plt.close()
