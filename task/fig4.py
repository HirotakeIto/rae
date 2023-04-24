import os
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from src.setting import setting


PATH = setting.path.path_vital_stat
PATH_RESULT_FOLDER = setting.path.path_result_folder
PATH_SAVE = os.path.join(PATH_RESULT_FOLDER, "fig", "fig4_vital_stat", "fig4_vital_stat.pdf")
TARGETS = [
    "age_father", "age_mother", "no_legitimate_100",
    "gram", "pregnant_week", "single_womb_100", "firstborn_100", "girl_100",
    "job1_100", "job2_100", "job3_100", "job4_100", "job5_100", "job6_100", "job7V_100",
    "job3_job4_100"
]
RENAME = {
    "age_father": "Father's age",
    "age_mother": "Mother's age",
    "no_legitimate_100": "Unmarried (%)",
    "gram": "Birth weight (gram)",
    "pregnant_week": "Gestational age (week)",
    "single_womb_100": "Singleton (%)",
    "firstborn_100": "First child (%)",
    "girl_100": "Girl (%)",
    "job1_100": "Farmer (%)",
    "job2_100": "Self-employed (%)",
    "job3_100": "Regular employment (small firm) (%)",
    "job4_100": "Regular employment (large firm) (%)",
    "job5_100": "Non-regular employment (%)",
    "job6_100": "No job (%)",
    "job7V_100": "Unknown (%)",
    "job3_job4_100": "Regular employment (%)",
}


def relative_age_to_birth_month(relative_age: float) -> float:
    if pd.isna(relative_age):
        return np.NaN
    return 3 - relative_age if relative_age < 3 else 15 - relative_age


def is_jobX(x, job_type):
    if pd.isna(x):
        return np.nan
    elif x in job_type:
        return 100
    else:
        return 0


def main():
    df = (
        pd.read_csv(PATH)
        .assign(
            birth_month=lambda dfx: dfx["relative_age"].apply(relative_age_to_birth_month),
            no_legitimate_100=lambda dfx: dfx["no_legitimate"] * 100,
            single_womb_100=lambda dfx: dfx["single_womb"] * 100,
            firstborn_100=lambda dfx: dfx["firstborn"] * 100,
            girl_100=lambda dfx: dfx["girl"] * 100,
            job1_100=lambda dfx: dfx["job"].apply(lambda x: is_jobX(x, ["job1"])),
            job2_100=lambda dfx: dfx["job"].apply(lambda x: is_jobX(x, ["job2"])),
            job3_100=lambda dfx: dfx["job"].apply(lambda x: is_jobX(x, ["job3"])),
            job4_100=lambda dfx: dfx["job"].apply(lambda x: is_jobX(x, ["job4"])),
            job5_100=lambda dfx: dfx["job"].apply(lambda x: is_jobX(x, ["job5"])),
            job6_100=lambda dfx: dfx["job"].apply(lambda x: is_jobX(x, ["job6"])),
            job7V_100=lambda dfx: dfx["job"].apply(lambda x: is_jobX(x, ["job7", "jobV"])),
            job3_job4_100=lambda dfx: dfx["job3_100"] + dfx["job4_100"],
        )
    )
    df.groupby("relative_age")[TARGETS].mean().to_csv("tmp.csv")
    ncols = 3
    nrows = len(TARGETS)//3 + 1 if len(TARGETS) % 3 != 0 else len(TARGETS)//3
    fig, axis = plt.subplots(nrows=nrows, ncols=ncols, figsize=(ncols*4, nrows*2.8), sharex=False, sharey=False)
    for i, target in enumerate(TARGETS):
        ax = axis[i // ncols, i % ncols]
        sns.pointplot(
            data=df,
            x="relative_age", y=target,
            ci=None, alpha=.6, height=6, ax=ax
        )
        # xticklabel_mapping = df[['relative_age', 'birth_month']].set_index('relative_age')['birth_month'].drop_duplicates().to_dict()
        ax.set_xticklabels([3, 2, 1, 12, 11, 10, 9, 8, 7, 6, 5, 4])
        [spin.set_visible(False) for spin in list(ax.spines.values())]
        ax.set_ylabel(None)
        ax.set_xlabel("Month of Birth")
        ax.set_title(RENAME[target])
        # ax.axvline(2.5, color="grey")
    else:
        if i != ncols * nrows:
            for j in range(i + 1, ncols * nrows):
                ax = axis[j // ncols, j % ncols]
                [spin.set_visible(False) for spin in list(ax.spines.values())]
                ax.set_xticks([])
                ax.set_yticks([])
    # plt.show()
    plt.subplots_adjust(hspace=0.4, wspace=0.2)
    os.makedirs(os.path.dirname(PATH_SAVE), exist_ok=True)
    plt.savefig(PATH_SAVE, facecolor="none", bbox_inches="tight")
    plt.close()
