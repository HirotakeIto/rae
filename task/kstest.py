import calendar
import os
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from scipy import stats, special as sc
from src.setting import setting

PATH = setting.path.path_vital_stat
PATH_RESULT_FOLDER = setting.path.path_result_folder
DIR_SAVE_CSV = os.path.join(PATH_RESULT_FOLDER, "kstest")
DIR_SAVE_PLOT = os.path.join(PATH_RESULT_FOLDER, "fig", "kstest")


def birth_month(relative_age):
    if pd.isnull(relative_age):
        return np.NaN
    elif relative_age < 3:
        return 3 - relative_age
    else:
        return 15 - relative_age


def cilen(arr, alpha=0.95):
    if len(arr) <= 1:
        return 0
    m, e, df = np.mean(arr), stats.sem(arr), len(arr) - 1
    interval = stats.t.interval(alpha, df, loc=m, scale=e)
    cilen = np.max(interval) - np.mean(interval)
    return cilen


def main():
    ##########
    # 日あたりの出生割合
    # （経緯：Vital Stat
    ##########
    # download
    df: pd.DataFrame = (
        pd.read_csv(PATH)
        .assign(birth_month=lambda dfx: dfx['relative_age'].apply(birth_month))
    )
    # setup month_to_error_bar
    dict_month_to_error_bar = {}
    for month in range(1, 13):
        error_bar = cilen(
            (df.loc[df["birth_month"].notnull(), "birth_month"] == month) * 1
            )
        dict_month_to_error_bar.update(
            {month: error_bar}
        )
    # setup dataframe for plot
    df_plot: pd.DataFrame = (
        df['birth_month']
        .value_counts().rename('month_count').rename_axis('month').reset_index().sort_values('month')
        .assign(
            month_error_bar=lambda dfx: dfx['month'].apply(lambda m: dict_month_to_error_bar[m]),
            day_in_month=lambda dfx: dfx['month'].apply(lambda x: calendar.monthrange(2018, int(x))[1]),
            day_birth_ratio=lambda dfx: dfx['month_count'] / dfx['month_count'].sum() / dfx['day_in_month'],
            day_error_bar=lambda dfx: dfx['month_error_bar'] / dfx['day_in_month'],
            day_birth_ratio_scale_month=lambda dfx: dfx["day_birth_ratio"] * 30,
            day_error_bar_scale_month=lambda dfx: dfx["day_error_bar"] * 30,
        )
    )
    null_day_birth_ratio = 1/365
    null_day_birth_ratio_scale_month = null_day_birth_ratio * 30
    # save
    os.makedirs(DIR_SAVE_CSV, exist_ok=True)
    df_plot.to_csv(os.path.join(DIR_SAVE_CSV, 'vital_stat_month.csv'), encoding='sjis', index=False, float_format='%.20f')
    # plot
    fig, ax = plt.subplots(1, 1, figsize=(10, 5))
    pallet = sns.color_palette(palette=['#3498db'], n_colors=1)  # '#e67e22'
    sns.barplot(
        df_plot['month'].values, df_plot['day_birth_ratio_scale_month'].values, yerr=df_plot['day_error_bar_scale_month'].values,
        palette=pallet, errcolor='gray', ax=ax
    )
    new_xticks = ax.get_xticks()
    new_xticklabels = [
        calendar.month_abbr[int(float(tick._text))]
        for tick in ax.get_xticklabels()
    ]
    ax.set_xticks(new_xticks)
    ax.set_xticklabels(new_xticklabels, fontsize=15)
    new_yticks = ax.get_yticks()
    new_yticklabels = [
        '{:.2%}'.format(float(tick))
        for tick in new_yticks
    ]
    ax.set_yticks(new_yticks)
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda d, pos: '{:.2%}'.format(d)))
    ax.set_yticklabels(new_yticklabels, fontsize=15)
    ax.hlines(
        null_day_birth_ratio_scale_month, color='#e67e22', linewidth=5,
        xmin=new_xticks.min() - 1, xmax=new_xticks.max() + 1
        )
    ax.vlines(2.5, ymin=0, ymax=df_plot['day_birth_ratio_scale_month'].max(), linestyle='--')  # vline 0始まりだから2.5でちょうど4月を表す。
    ax.text(2.5, df_plot['day_birth_ratio_scale_month'].max(), "Grade cutoff", fontsize='large')
    ax.set_xlabel('', fontsize=20)
    ax.set_ylabel('', fontsize=20)
    ax.set_facecolor('none')
    [spin.set_visible(False) for spin in list(ax.spines.values())]
    os.makedirs(DIR_SAVE_PLOT, exist_ok=True)
    plt.savefig(os.path.join(DIR_SAVE_PLOT, 'distribution_birth_month_from_vital_stats.png'), facecolor='none', bbox_inches="tight", edgecolor="none")
    plt.close()
