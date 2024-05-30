import pandas as pd
from pandas import DataFrame


def remove_col(df) -> DataFrame:
    """
    To remove field from the data if category is less than 2
    :param df: Dataframe
    return: Dataframe
    """
    collect = []
    for col in df.columns:
        if df[col].nunique() < 2:
            collect.append(col)
    return df.drop(collect, axis=1)


def preprocess_data(data_dict: dict) -> dict:
    """
    Pre-process data represented as a dictionary of DataFrame.
    :param data_dict:  The dictionary of dataframe
    """
    prep_data_dict = {}
    for dataset_name, df in data_dict.items():
        data = remove_col(df)
        prep_data_dict[dataset_name] = data

    return prep_data_dict
