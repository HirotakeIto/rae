import pandas as pd
import os
from collections import namedtuple
from typing import List

path_main = "./data/dataset1.csv"
path_result_folder = "./result/"
path_result = os.path.join(path_result_folder, "desc", "desc.csv")


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


def main():
    df = pd.read_csv(path_main)
    summary: pd.DataFrame = (
        pd.DataFrame()
        .append(
            df.pipe(descriptive).assign(relative_age='all')
        )
        .append(
            df.groupby(['relative_age']).apply(descriptive).reset_index(drop=False)
        )
        [["level_1", "name", "relative_age", "species", "value"]]
    )
    os.makedirs(os.path.dirname(path_result), exist_ok=True)
    summary.to_csv(path_result, index=False)
