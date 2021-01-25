import pandas as pd
import gc
import calendar
from scipy import stats
import scipy.special as sc
from scipy.stats import distributions
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import seaborn as sns
import os
from scipy import stats
import numpy as np

def cilen(arr, alpha=0.95):
    if len(arr) <= 1:
        return 0
    m, e, df = np.mean(arr), stats.sem(arr), len(arr) - 1
    interval = stats.t.interval(alpha, df, loc=m, scale=e)
    # import pdb;pdb.set_trace()
    cilen = np.max(interval) - np.mean(interval)
    return cilen

def saitama():
    # data
    df = pd.read_csv('./notebooks/Yamaguchi/RAE/data/dataset1.csv')
    df_use = df.loc[df['q138'].between(1, 12), ['year', 'grade', 'q138']]
    gc.collect()
    ##########
    # 月あたりの出生割合
    ##########
    distribution = df_use['q138'].value_counts().rename('count').rename_axis('month').reset_index().sort_values('month')
    distribution['ratio'] = distribution['count']/distribution['count'].sum()
    distribution['ratio_cumsum'] = distribution['ratio'].cumsum()
    # null data
    df_null_hypthesis = pd.DataFrame(
        [[month, calendar.monthrange(2018, month)[1]] for month in range(1, 13)],
        columns=['month', 'null']
    )
    df_null_hypthesis = df_null_hypthesis.sort_values('month')
    df_null_hypthesis['null_ratio'] = df_null_hypthesis['null']/df_null_hypthesis['null'].sum()
    df_null_hypthesis['null_ratio_cumsum'] = df_null_hypthesis['null_ratio'].cumsum()
    # test
    df_use_kstest = pd.merge(distribution,
                             df_null_hypthesis, on='month').sort_values('month')
    number_of_trials = df_use_kstest['count'].sum()
    # binom_test
    df_use_kstest['binom_test_pval'] = df_use_kstest.apply(
        lambda x: stats.binom_test(
            x['count'], number_of_trials, p=x['null_ratio']),
        axis=1)
    df_use_kstest['Dval'] = (df_use_kstest['ratio_cumsum'] -df_use_kstest['null_ratio_cumsum']).abs()
    Dval_max = df_use_kstest['Dval'].max()
    D_1percent = sc.smirnovi(len(df_use), 0.005)
    D_pval = sc.smirnov(number_of_trials, Dval_max)
    wei = pd.DataFrame([[Dval_max, D_pval]], columns=['Dval_max', 'Dval_pval'])
    result = pd.concat([df_use_kstest, wei], axis=0)[list(df_use_kstest.columns) + list(wei.columns)]
    df_use_kstest.to_csv('res2.csv', encoding='sjis', index=False, float_format='%.20f')
    result.to_csv('res.csv', encoding='sjis', index=False, float_format='%.20f')

    ##########
    # 日あたりの出生割合
    # （経緯：埼玉県学力調査）
    ##########

    df_use_kstest = (
        df_use['q138']
        .value_counts().rename('month_count').rename_axis('month').reset_index().sort_values('month')
        .assign(
            month_error_bar = lambda dfx: dfx['month'].apply(
                lambda m: cilen(
                    df_use['q138'].where(
                        df_use['q138'].isnull(),
                        np.where(df_use['q138'] == m, 1, 0)
                    )
                )
            )
        )
        .assign(
            day_in_month = lambda dfx: dfx['month'].apply(lambda x: calendar.monthrange(2018, int(x))[1]),
        )
        .assign(
            day_birth_ratio = lambda dfx: dfx['month_count']/ dfx['month_count'].sum() / dfx['day_in_month'],
            day_error_bar = lambda dfx:dfx['month_error_bar'] / dfx['day_in_month'],
            month_birth_ratio = lambda dfx: dfx['month_count']/ dfx['month_count'].sum(),
            null_day_birth_ratio = 1/365,
            null_month_birth_ratio = lambda dfx:dfx['day_in_month']/dfx['day_in_month'].sum(),
        )
        .assign(
            binom_test_pval = lambda dfx: dfx.apply(
                lambda x: stats.binom_test(x['month_count'], dfx['month_count'].sum(), p=x['null_month_birth_ratio']),
                axis=1
            )
        )
    )
    folder = 'notebooks/Yamaguchi/RAE/result/top/kstest'
    folder2 = 'notebooks/Yamaguchi/RAE/result/top/fig/balance'
    os.makedirs(folder, exist_ok=True)
    os.makedirs(folder, exist_ok=True)
    df_use_kstest.to_csv(os.path.join(folder, 'res.csv'), encoding='sjis', index=False, float_format='%.20f')
    df_plot = (
        df_use_kstest
        # .melt(id_vars='month', value_vars=['day_birth_ratio', 'null_day_birth_ratio'])
        # .rename(columns={'day_birth_ratio': 'Observed', 'null_day_birth_ratio': 'Uniform'})
    )
    fig, axs = plt.subplots(1, 1, figsize=(10, 5))
    ax = axs
    pallet = sns.color_palette(palette=['#3498db'], n_colors=1)  # '#e67e22'
    sns.barplot(
        df_plot['month'], df_plot['day_birth_ratio'], yerr=df_plot['day_error_bar'],
        palette=pallet, errcolor = 'gray', ax=ax)

    aa = ax.get_xticklabels()
    new_xticks = ax.get_xticks()
    new_xticklabels = [
        calendar.month_abbr[int(float(tick._text))]
        for tick in ax.get_xticklabels()
    ]
    new_yticks = ax.get_yticks()
    new_yticklabels =  [
        '{:.2%}'.format(float(tick))
        for tick in new_yticks
    ]
    x_line = list(ax.get_xbound())
    y_line = [df_plot['null_day_birth_ratio'].unique()[0]] * len(x_line)
    sns.lineplot(x_line, y_line, color='#e67e22', ax=ax, linewidth=5)
    ax.set_xlabel('', fontsize=20)
    ax.set_ylabel('', fontsize=20)
    ax.set_facecolor('none')
    ax.set_xticks(new_xticks)
    ax.set_xticklabels(new_xticklabels, fontsize=15)
    ax.set_yticks(new_yticks)
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda d,pos: '{:.2%}'.format(d)))
    ax.set_yticklabels(new_yticklabels, fontsize=15)
    # ax.legend(bbox_to_anchor=(0.5, -0.15), loc='center', borderaxespad=0, fontsize=15, frameon=False, ncol=2)
    [spin.set_visible(False) for spin in list(ax.spines.values())]
    plt.savefig(os.path.join(folder2,'distribution_birth_month.png'), facecolor='none', bbox_inches="tight", edgecolor="none")
    plt.show()
    plt.close()


##########
# 日あたりの出生割合
# （経緯：Vital Stat
##########

# check
df2 = (
    pd.read_csv('notebooks/Yamaguchi/RAE/data/saitama_birth.csv')
    .assign(
        q138 = lambda dfx:
            dfx['relative_age'].where(
                dfx['relative_age'].isnull(), pd.np.where(
                    dfx['relative_age'] < 3, 3 - dfx['relative_age'], 15 - dfx['relative_age']
                )
            )
    )
)
df_use_kstest = (
    df2['q138']
    .value_counts().rename('month_count').rename_axis('month').reset_index().sort_values('month')
    .assign(
        month_error_bar = lambda dfx: dfx['month'].apply(
            lambda m: cilen(
                df2['q138'].where(
                    df2['q138'].isnull(),
                    np.where(df2['q138'] == m, 1, 0)
                )
            )
        )
    )
    .assign(
        day_in_month = lambda dfx: dfx['month'].apply(lambda x: calendar.monthrange(2018, int(x))[1]),
    )
    .assign(
        day_birth_ratio = lambda dfx: dfx['month_count']/ dfx['month_count'].sum() / dfx['day_in_month'],
        day_error_bar = lambda dfx:dfx['month_error_bar'] / dfx['day_in_month'],
        month_birth_ratio = lambda dfx: dfx['month_count']/ dfx['month_count'].sum(),
        null_day_birth_ratio = 1/365,
        null_month_birth_ratio = lambda dfx:dfx['day_in_month']/dfx['day_in_month'].sum(),
    )
    .assign(
        binom_test_pval = lambda dfx: dfx.apply(
            lambda x: stats.binom_test(x['month_count'], dfx['month_count'].sum(), p=x['null_month_birth_ratio']),
            axis=1
        )
    )
)
folder = 'notebooks/Yamaguchi/RAE/result/top/kstest'
folder2 = 'notebooks/Yamaguchi/RAE/result/top/fig/balance'
os.makedirs(folder, exist_ok=True)
os.makedirs(folder, exist_ok=True)
df_use_kstest.to_csv(os.path.join(folder, 'vital_stat_month.csv'), encoding='sjis', index=False, float_format='%.20f')
df_plot = (
    df_use_kstest
    # .melt(id_vars='month', value_vars=['day_birth_ratio', 'null_day_birth_ratio'])
    # .rename(columns={'day_birth_ratio': 'Observed', 'null_day_birth_ratio': 'Uniform'})
    .assign(
        day_birth_ratio_scale_month = lambda dfx: dfx["day_birth_ratio"] * 30,
        day_error_bar_scale_month = lambda dfx: dfx["day_error_bar"] * 30,
        null_day_birth_ratio_scale_month = lambda dfx: dfx["null_day_birth_ratio"] * 30
    )
)
fig, axs = plt.subplots(1, 1, figsize=(10, 5))
ax = axs
pallet = sns.color_palette(palette=['#3498db'], n_colors=1)  # '#e67e22'
sns.barplot(
    df_plot['month'].values, df_plot['day_birth_ratio_scale_month'].values, yerr=df_plot['day_error_bar_scale_month'].values,
    palette=pallet, errcolor = 'gray', ax=ax
)

aa = ax.get_xticklabels()
new_xticks = ax.get_xticks()
new_xticklabels = [
    calendar.month_abbr[int(float(tick._text))]
    for tick in ax.get_xticklabels()
]
new_yticks = ax.get_yticks()
new_yticklabels =  [
    '{:.2%}'.format(float(tick))
    for tick in new_yticks
]
x_line = list(ax.get_xbound())
y_line = [df_plot['null_day_birth_ratio_scale_month'].unique()[0]] * len(x_line)
sns.lineplot(x_line, y_line, color='#e67e22', ax=ax, linewidth=5)
# vline 0始まりだから2.5でちょうど4月を表す。
ax.vlines(2.5, ymin=0, ymax=df_plot['day_birth_ratio_scale_month'].max(), linestyle='--')
ax.text(2.5, df_plot['day_birth_ratio_scale_month'].max(), "Grade cutoff", fontsize='large')
ax.set_xlabel('', fontsize=20)
ax.set_ylabel('', fontsize=20)
ax.set_facecolor('none')
ax.set_xticks(new_xticks)
ax.set_xticklabels(new_xticklabels, fontsize=15)
ax.set_yticks(new_yticks)
ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda d,pos: '{:.2%}'.format(d)))
ax.set_yticklabels(new_yticklabels, fontsize=15)
# ax.legend(bbox_to_anchor=(0.5, -0.15), loc='center', borderaxespad=0, fontsize=15, frameon=False, ncol=2)
[spin.set_visible(False) for spin in list(ax.spines.values())]
plt.savefig(os.path.join(folder2,'distribution_birth_month_from_vital_stats.png'), facecolor='none', bbox_inches="tight", edgecolor="none")
plt.show()
plt.close()
