import pandas as pd
from functools import reduce
import os
from collections import namedtuple
from typing import List

def descriptive(df_x):
    df_size = pd.DataFrame([['samplesize', 'samplesize', df_x.shape[0]]], columns=['col', 'index', 'count'])
    df_g = df_x['grade'].value_counts(normalize=False).rename('count').reset_index().assign(col = 'grade')
    df_s = df_x['sex'].value_counts(normalize=True).rename('count').reset_index().assign(col = 'sex')
    df_l = df_x['lowses'].value_counts(normalize=True).rename('count').reset_index().assign(col = 'lowses')
    use_list = [
        'math_level', 'kokugo_level', 'eng_level',
        'strategy', 'selfcontrol', 'selfefficacy', 'dilligence',
        'teacherrelation', 'teacherrelation2', 'friendrelation',
        'hoursprep', 'hourshome', 'studytime', 'cram',
        'zmath_level', 'zkokugo_level', 'zeng_level',
        'zfriendrelation',
        'zstrategy', 'zselfcontrol', 'zselfefficacy', 'zdilligence',
        'teacherrelation_primitive', 'teacherrelation2_primitive',
        'dilligence_scaled', 'selfcontrol_scaled', 'selfefficacy_scaled',
        'reading_time_input_weekdays', 'smart_phone_gaming_tv_time', 'lesson_time', 'playing_sport'
    ]
    df_u = df_x[use_list].mean().rename('mean').reset_index().assign(col = lambda dfx: dfx['index'])
    df_std = df_x[use_list].std().rename('std').reset_index().assign(col=lambda dfx: dfx['index'])
    return (
        reduce(lambda x, y: x.append(y), [df_size, df_g, df_s, df_l, df_u, df_std])
        .reset_index(drop=True)
    )


def descriptive(df_x) -> pd.DataFrame:
    Row = namedtuple('Row', ['name', 'species', 'value'])
    def seriesx_to_rows(seriesx: pd.Series, name='aff') -> List[Row]:
        return [
            Row(name=name, species='count', value=seriesx.count()),
            Row(name=name, species='mean', value=seriesx.mean()),
            Row(name=name, species='std', value=seriesx.std()),
        ]

    def seriesx_to_rows_count(seriesx: pd.Series, name='aff', normalize=True, fmt=None) -> List[Row]:
        fmt = '{name}_{group}' if fmt is None else fmt
        rows = []
        for (group, value) in seriesx.value_counts(normalize=normalize).reset_index().values:
            name_this = fmt.format(name=name, group=group)
            rows += [Row(name=name_this, species='count', value=value)]
        return rows

    use_list = [
        'math_level', 'kokugo_level', 'eng_level',
        'strategy', 'selfcontrol', 'selfefficacy', 'dilligence',
        'teacherrelation', 'teacherrelation2', 'friendrelation',
        'hoursprep', 'hourshome', 'studytime', 'cram',
        'zmath_level', 'zkokugo_level', 'zeng_level',
        'zfriendrelation',
        'zstrategy', 'zselfcontrol', 'zselfefficacy', 'zdilligence',
        'teacherrelation_primitive', 'teacherrelation2_primitive',
        'dilligence_scaled', 'selfcontrol_scaled', 'selfefficacy_scaled',
        'reading_time_in_a_weekdays', 'smart_phone_gaming_tv_time', 'lesson_time', 'playing_sport'
    ]
    rows = []
    rows += [Row('samplesize', 'samplesize', df_x.shape[0])]
    for target in use_list:
        rows += seriesx_to_rows(df_x[target], name=target)
    for target in ['grade', 'sex', 'lowses']:
        rows += seriesx_to_rows_count(df_x[target], name=target, normalize=True)
    for target in ['grade']:
        rows += seriesx_to_rows_count(df_x[target], name=target + '_rawcount', normalize=False)
    return pd.DataFrame(rows)


df = pd.read_csv('./notebooks/Yamaguchi/RAE/data/dataset1.csv')

# # count
# df.groupby(['relative_age']).size().rename('value').reset_index()
# df.groupby(['relative_age'])['grade'].value_counts(normalize=True).rename('value').reset_index()
# df.groupby(['relative_age'])['sex'].value_counts(normalize=True).rename('value').reset_index()
# df.groupby(['relative_age'])['lowses'].value_counts(normalize=True).rename('value').reset_index()
# df.groupby(['relative_age'])[['zmath_level', 'zkokugo_level']].mean().reset_index().melt('relative_age')


a = df.pipe(descriptive).assign(relative_age = 'all')
b = df.groupby(['relative_age']).apply(descriptive).reset_index(drop=False)
path = 'notebooks/Yamaguchi/RAE/result/top/desc'
os.makedirs(path, exist_ok=True)
_ = (
    reduce(lambda x, y: x.append(y), [a, b])
    .to_csv(os.path.join(path, 'desc.csv'), index=False)
)


def check():
    # attrition rate
    mst_id_2015 = df.loc[df["year"] == 2015, "mst_id"].unique()
    mst_id_2016 = df.loc[df["year"] == 2016, "mst_id"].unique()
    mst_id_2017 = df.loc[df["year"] == 2017, "mst_id"].unique()
    mst_id_2018 = df.loc[df["year"] == 2018, "mst_id"].unique()
    df['is_attrition'] = pd.np.NaN
    df.loc[df['year'] == 2015, "is_attrition"] = (~ df["mst_id"].isin(mst_id_2016)) * 1
    df.loc[df['year'] == 2016, "is_attrition"] = (~ df["mst_id"].isin(mst_id_2017)) * 1
    df.loc[df['year'] == 2017, "is_attrition"] = (~ df["mst_id"].isin(mst_id_2018)) * 1
    df.loc[df['year'] == 2018, "is_attrition"] = pd.np.NaN
    df.groupby(["grade", "year"])["is_attrition"].mean().apply(lambda x: '{:.2%}'.format(x))
    df.dropna(subset=['is_attrition']).groupby(["grade"])["is_attrition"].mean().apply(lambda x: '{:.2%}'.format(x))
    '{:.0%}'.format(0.12345)



