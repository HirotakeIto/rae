"""
https://peereffect.slack.com/archives/C8XBQ6VNK/p1601021135006100?thread_ts=1600678443.014800&cid=C8XBQ6VNK
生まれ月の分布を学年ごとにみる
生まれ月の分布が隣接する２つの学年で異なるか検定（2-sample KS test）
生まれ月別離脱率を学年ごとにみる
"""
import os
import pandas as pd
from datatable import fread
from scipy.stats import ks_2samp
from collections import namedtuple
from src.setting import setting
PATH_MAIN = setting.path.path_main
PATH_RESULT_FOLDER = setting.path.path_result_folder
DIR_SAVE = os.path.join(PATH_RESULT_FOLDER, "t33")


def get_relative_age_value_counts_by_grade(dfx: pd.DataFrame) -> pd.DataFrame:  # DataFrame[["grade", "relative_age", "count"]]
    return (
        dfx
        .pipe(lambda dfx: dfx[dfx["year"] >= 2016])
        .groupby(['grade'])['relative_age']
        .value_counts(normalize=True).rename("count").reset_index()
        [["grade", "relative_age", "count"]]
    )


Result = namedtuple("Result", ["grade1", "grade2", "statistic", "pvalue"])


def test_is_different_distribution(dfx: pd.DataFrame) -> pd.DataFrame:  # DataFrame[["grade1", "grade2", "statistic", "pvalue"]]
    dfx_use = (
        dfx
        .pipe(lambda dfx: dfx[dfx["year"] >= 2016])
        [["grade", "relative_age"]]
    )
    results = []
    for grade1, grade2 in zip(range(4, 8), range(5, 9)):
        data1 = dfx_use.pipe(lambda dfxx: dfxx[(dfxx["grade"] >= grade1)])["relative_age"]
        data2 = dfx_use.pipe(lambda dfxx: dfxx[(dfxx["grade"] >= grade2)])["relative_age"]
        statistic, pvalue = ks_2samp(data1=data1, data2=data2)  # the null hypothesis that 2 independent samples are drawn from the same continuous distribution
        results.append(Result(grade1=grade1, grade2=grade2, statistic=statistic, pvalue=pvalue))
    return (
        pd.DataFrame(results)
        [["grade1", "grade2", "statistic", "pvalue"]]
    )


def add_is_attrition_next_year(dfx: pd.DataFrame):
    year_min = int(dfx['year'].min())
    year_max = int(dfx['year'].max())
    years = range(year_min, year_max + 1)
    dict_year_to_mst_id = {}
    for year1, year2 in zip(years[:-1], years[1:]):
        print(year1, year2)
        mst_ids_year1 = set(dfx.pipe(lambda dfxx: dfxx[dfxx["year"] == year1])["mst_id"].unique().tolist())
        mst_ids_year2 = set(dfx.pipe(lambda dfxx: dfxx[dfxx["year"] == year2])["mst_id"].unique().tolist())
        mst_ids_in_year1_not_in_year2 = mst_ids_year1 - mst_ids_year2
        dict_year_to_mst_id.update({year1: mst_ids_in_year1_not_in_year2})

    def is_attrition_next_year(year, mst_id, grade):
        if pd.isna(year) | pd.isna(mst_id):
            return pd.np.NaN
        if year not in dict_year_to_mst_id:
            return pd.np.NaN
        if grade == 9:
            return pd.np.NaN
        mst_ids = dict_year_to_mst_id[year]
        if mst_id in mst_ids:
            return 1
        else:
            return 0

    dfx["is_attrition_next_year"] = pd.np.vectorize(is_attrition_next_year, otypes=["float"])(
        year=dfx["year"], mst_id=dfx["mst_id"], grade=dfx["grade"]
        )
    return dfx


def get_is_attrition_next_year_mean_by_grade(dfx: pd.DataFrame) -> pd.DataFrame:  # DataFrame[["grade", "relative_age", "count"]]
    return (
        dfx
        .pipe(add_is_attrition_next_year)
        # .pipe(lambda dfx: dfx[dfx["year"] >= 2016])
        .groupby(['grade', "relative_age"])['is_attrition_next_year']
        .mean().reset_index()
        [["grade", "relative_age", "is_attrition_next_year"]]
    )


def main():
    df = (
        fread(file=PATH_MAIN)
        .to_pandas()
    )
    os.makedirs(DIR_SAVE, exist_ok=True)
    (
        get_relative_age_value_counts_by_grade(dfx=df)
        .to_csv(os.path.join(DIR_SAVE, "relative_age_value_counts.csv"), index=False)
    )
    (
        test_is_different_distribution(dfx=df)
        .to_csv(os.path.join(DIR_SAVE, "test_is_different_distribution.csv"), index=False)
    )
    (
        get_is_attrition_next_year_mean_by_grade(dfx=df)
        .to_csv(os.path.join(DIR_SAVE, "is_attrition_next_year_mean.csv"), index=False)
    )
