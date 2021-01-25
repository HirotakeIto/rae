from matplotlib import pyplot as plt
import pandas as pd
import os
import re
from src.lib.mapper import ColumnNameEnglishRegexMapper, MapperProvider
from collections import defaultdict
import datatable as dt


def find_matching_regexen(word, dicts=dict):
    return [description for regex, description in dicts.items() if re.compile(regex).match(word)]
dir_save = os.path.join('notebooks/Yamaguchi/RAE/result/top/fig', 'absolute_age_score')

df = (
    dt.fread('./notebooks/Yamaguchi/RAE/data/dataset1.csv')
    .to_pandas()
)
fetch_col = [
    'kokugo_level', 'math_level', 'eng_level',
    'dilligence_scaled', 'selfcontrol_scaled', 'selfefficacy_scaled',
    'studytime', 'reading_time_in_a_weekdays', 'cram', 'playing_sport', 'lesson_time',
    'teacherrelation2_primitive', 'friendrelation',
    # 'zyunan', 'planning', 'execution', 'resource', 'ninti', 'effort',
]

mapper = MapperProvider([ColumnNameEnglishRegexMapper()]).provide()
mapper.update({
    'kokugo_level': 'Japanese',
    'math_level': 'Math',
    'eng_level': "English",
    'dilligence_scaled': 'Conscientiousness',
    'selfcontrol_scaled': 'Self-control',
    'selfefficacy_scaled': 'Self-efficacy',
    'studytime': 'Weekly hours of studying outside school',
    'reading_time_in_a_weekdays': 'Weekly hours of reading',
    'cram': 'Prep school participation rate',
    'playing_sport': 'Weekly hours of playing outside and sports',
    'lesson_time': 'Weekly hours of arts, music, and sports',
    'smart_phone_gaming_tv_time': 'Mobile phone, game, TV',
    'teacherrelation2_primitive': 'Relationship with teachers',
    'friendrelation': 'Relationship with peers',
})

ylabel_mapper = defaultdict(lambda : "Score")
ylabel_mapper.update({
    'studytime': 'Hours',
    'reading_time_in_a_weekdays': 'Hours',
    'cram': 'Rate',
    'playing_sport': 'Hours',
    'lesson_time': 'Hours',
})


df_plot = (
    df
    .assign(absolute_age = lambda dfx: dfx['relative_age'] + (dfx['grade'] + 5) * 12)
    # .pipe(
    #     lambda dfx: dfx.loc[dfx['year'] == 2018, :]
    # )
    .groupby(['grade', 'absolute_age'])
    [fetch_col].mean().reset_index()
)
## All FIGURE
fig, axs = plt.subplots(len(fetch_col)//4 + 1, 4, figsize=(10*(len(fetch_col)//4 + 1), 7*(4)))
for i, subject in enumerate(fetch_col):
    ax = axs[i//4, i%4]
    for grade in df_plot['grade'].unique().tolist():
        df_plot_slice = df_plot.loc[(df_plot['grade'] == grade), ['absolute_age', subject]].dropna()
        if df_plot_slice.shape[0] == 0:
            continue
        ax.scatter(
            df_plot_slice['absolute_age'],
            df_plot_slice[subject],
            label='Grade {i}'.format(i=int(grade)))
    [spin.set_visible(False) for spin in list(ax.spines.values())]
    ax.set_xlabel('Age (Month)', fontsize=30)
    ax.set_ylabel('Score', fontsize=30)
    titles = find_matching_regexen(subject, mapper)
    if len(titles) != 0:
        print(titles)
    ax.set_title(titles[0], fontsize=30)
    ax.legend(frameon=False, fontsize=30, loc="upper left", bbox_to_anchor=(1, 1))
    ax.tick_params(labelsize=15)
    ax.set_xlim(df_plot.absolute_age.min()-1, df_plot.absolute_age.max()+1)
    if subject in ['gakuryoku', 'kokugo_level', 'math_level', 'eng_level']:
        ax.set_ylim(-1.5, 1.5)
    # if subject in ['playing_sport', 'lesson_time', "studytime", "reading_time_in_a_weekdays"]:
    #     ax.set_ylim(1, 12)
else:
    for i in range(len(fetch_col), (len(fetch_col)//4 + 1)*4):
        ax = axs[i // 4, i % 4]
        [spin.set_visible(False) for spin in list(ax.spines.values())]
        ax.set_xticks([])
        ax.set_yticks([])
fig.suptitle('Absolute age and score')
os.makedirs(dir_save, exist_ok=True)
plt.subplots_adjust(hspace = 0.5, wspace = 0.5)
plt.savefig(os.path.join(dir_save, 'fig1.pdf'), facecolor='none', bbox_inches="tight")
plt.show()
plt.close()

# Individual Figure
marker_list = ['.', '^', 's', '*', '+', 'x']
for i, subject in enumerate(fetch_col):
    fig, axs = plt.subplots(1, 1, figsize=(10, 7))
    ax = axs
    for j , grade in enumerate(df_plot['grade'].unique().tolist()):
        df_plot_slice = df_plot.loc[(df_plot['grade'] == grade), ['absolute_age', subject]].dropna()
        if df_plot_slice.shape[0] == 0:
            continue
        ax.scatter(
            df_plot_slice['absolute_age'],
            df_plot_slice[subject],
            label='Grade {i}'.format(i=int(grade)),
            marker = marker_list[j]
        )
    [spin.set_visible(False) for spin in list(ax.spines.values())]
    ax.set_xlabel('Age (month)', fontsize=30)
    ax.set_ylabel(ylabel_mapper[subject], fontsize=30)
    ax.set_title(mapper[subject], fontsize=30)
    ax.set_facecolor('none')
    ax.legend(frameon=False, fontsize=30, loc="upper left", bbox_to_anchor=(1, 1))
    ax.tick_params(labelsize=15)
    ax.set_xlim(df_plot.absolute_age.min()-1, df_plot.absolute_age.max()+1)
    if subject in ['gakuryoku', 'kokugo_level', 'math_level', 'eng_level']:
        ax.set_ylim(-1.5, 1.5)
    # if subject in ['playing_sport', 'lesson_time', "studytime", "reading_time_in_a_weekdays"]:
    #     ax.set_ylim(1, 12)
    [spin.set_visible(False) for spin in list(ax.spines.values())]
    os.makedirs(dir_save, exist_ok=True)
    plt.subplots_adjust(hspace = 0.5, wspace = 0.5)
    # filname = 'fig-{0}.png'.format(subject)
    # plt.savefig(os.path.join(dir_save, filname), facecolor='none', bbox_inches="tight")
    filname = 'fig-{0}.pdf'.format(subject)
    plt.savefig(os.path.join(dir_save, filname), facecolor='none', bbox_inches="tight")
    plt.show()
    plt.close()



def debug(df_plot: pd.DataFrame):
    dir_save = "./tmp"
    fetch_col = [
        'math_level',  'selfefficacy_scaled', 'friendrelation', 'studytime'
    ]
    mapper_tmp = mapper.copy()
    mapper_tmp.update({"math_level": "Cognitive Skill (math)", "selfefficacy_scaled": "Non-Cognitive Skill (self-efficacy)"})
    marker_list = ['.', '^', 's', '*', '+', 'x']
    for i, subject in enumerate(fetch_col):
        fig, axs = plt.subplots(1, 1, figsize=(9 * (11/12), 14 * (11/12)))
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
        ax.set_xlabel('Age (Month)', fontsize=25)
        ax.set_ylabel(ylabel_mapper[subject], fontsize=25)
        ax.set_title(mapper_tmp[subject], fontsize=25)
        ax.set_facecolor('none')
        ax.legend(frameon=False, fontsize=15, loc="upper left", bbox_to_anchor=(1, 1))
        ax.tick_params(labelsize=15)
        ax.set_xlim(df_plot.absolute_age.min() - 1, df_plot.absolute_age.max() + 1)
        if subject in ['gakuryoku', 'kokugo_level', 'math_level', 'eng_level']:
            ax.set_ylim(-1.5, 1.5)
        # if subject in ['playing_sport', 'lesson_time', "studytime", "reading_time_in_a_weekdays"]:
        #     ax.set_ylim(1, 12)
        [spin.set_visible(False) for spin in list(ax.spines.values())]
        os.makedirs(dir_save, exist_ok=True)
        plt.subplots_adjust(hspace=0.5, wspace=0.5)
        filname = 'fig-{0}.png'.format(subject)
        plt.savefig(os.path.join(dir_save, filname), facecolor='none', bbox_inches="tight")
        plt.show()
        plt.close()
