import os
import datatable as dt
from typing import Tuple
from matplotlib import pyplot as plt
from collections import defaultdict
from src.setting import setting
# setting
PATH_MAIN = setting.path.path_main
PATH_RESULT_FOLDER = setting.path.path_result_folder
DIR_SAVE = os.path.join(PATH_RESULT_FOLDER, "fig", "absolute_age_score")
COLS_SUBJECT = [
    'kokugo_level', 'math_level', 'eng_level',
    'dilligence_scaled', 'selfcontrol_scaled', 'selfefficacy_scaled',
    'studytime', 'reading_time_in_a_weekdays', 'cram', 'playing_sport', 'lesson_time',
    'teacherrelation2_primitive', 'friendrelation',
    # 'zyunan', 'planning', 'execution', 'resource', 'ninti', 'effort',
]
MAPPER_TITLE = setting.rename.figure_outcome_age.title
MAPPER_YLABEL = defaultdict(lambda: "Score")
MAPPER_YLABEL.update(setting.rename.figure_outcome_age.ylabel.custom)
XLABEL = 'Age (month)'


def get_ylim(subject: str) -> Tuple[float, float]:
    if subject in ['gakuryoku', 'kokugo_level', 'math_level', 'eng_level']:
        return (-1.5, 1.5)
    else:
        return (None, None)


def main():
    df = (
        dt.fread(PATH_MAIN)
        .to_pandas()
    )
    df_plot = (
        df
        .assign(absolute_age=lambda dfx: dfx['relative_age'] + (dfx['grade'] + 5) * 12)
        .groupby(['grade', 'absolute_age'])
        [COLS_SUBJECT].mean().reset_index()
    )
    # Individual Figure
    marker_list = ['.', '^', 's', '*', '+', 'x']
    for i, subject in enumerate(COLS_SUBJECT):
        fig, axs = plt.subplots(1, 1, figsize=(10, 7))
        ax = axs
        for j, grade in enumerate(df_plot['grade'].unique().tolist()):
            df_plot_slice = df_plot.loc[(df_plot['grade'] == grade), ['absolute_age', subject]].dropna()
            if df_plot_slice.shape[0] == 0:
                continue
            ax.scatter(
                df_plot_slice['absolute_age'],
                df_plot_slice[subject],
                label='Grade {i}'.format(i=int(grade)),
                marker=marker_list[j]
            )
        [spin.set_visible(False) for spin in list(ax.spines.values())]
        ax.set_xlabel(XLABEL, fontsize=30)
        ax.set_ylabel(MAPPER_YLABEL[subject], fontsize=30)
        ax.set_title(MAPPER_TITLE[subject], fontsize=30)
        ax.set_facecolor('none')
        ax.legend(frameon=False, fontsize=30, loc="upper left", bbox_to_anchor=(1, 1))
        ax.tick_params(labelsize=15)
        ax.set_xlim(df_plot.absolute_age.min()-1, df_plot.absolute_age.max()+1)
        bottom, top = get_ylim(subject=subject)
        ax.set_ylim(bottom=bottom, top=top)
        [spin.set_visible(False) for spin in list(ax.spines.values())]
        os.makedirs(DIR_SAVE, exist_ok=True)
        filname = 'fig-{0}.pdf'.format(subject)
        plt.savefig(os.path.join(DIR_SAVE, filname), facecolor='none', bbox_inches="tight")
        plt.close()
