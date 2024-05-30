import numpy as np
from pandas import DataFrame
from sklearn.preprocessing import OrdinalEncoder
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import train_test_split
from config.train_model_config import *



def divide_train_test(df: DataFrame, target: str) -> tuple:
    """
    divide data set in train and test data
    :param df: the pre-processed training data
    :param target: class name
    :return: input data for training, output data for training, input data for testing, output data for testing
    """
    df = df.copy()
    X = df.drop([target], axis=1)
    y = df[target]
    X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        train_size=0.8,
                                                        stratify=y,
                                                        random_state=RANDOM_STATE)
    return X_train, X_test, y_train, y_test


def model_loader_script() -> tuple:
    """
    get dictionary of model
    :return: dictionary of model
    """
    model_dict = {}
    for k, v in MODELS.items():
        model_dict[k] = v['model'](max_depth=v['params']['max_depth'],
                                           min_samples_leaf=v['params']['min_samples_leaf'],
                                           min_samples_split=v['params']['min_samples_split'],
                                           random_state=RANDOM_STATE)

    return model_dict


def categorical_encoding_loader_scrip(df: DataFrame) -> dict:
    """
    get dictionary of encodings for the pre-processed training data
    :param df: the pre-processed training data
    :return: dictionary of the categorical encodings
    """
    df = df.copy()
    cat_vars_cols = df.select_dtypes(include=['object']).columns
    ordinal_enc = OrdinalEncoder(dtype=np.int64)
    df[cat_vars_cols] = ordinal_enc.fit_transform(df[cat_vars_cols])
    encoding_dict = {}
    for i, cat in enumerate(df[cat_vars_cols].columns):
        val = ordinal_enc.categories_[i]
        encoding_dict[cat] = val
    return encoding_dict


def encode_categorical_fields(df: DataFrame, encoding_dict: dict) -> DataFrame:
    """
    map categorical encodings to the data
    :param df: the data
    :param encoding_dict: dictionary of the categorical encodings
    :return: data with encoded categorical features
    """
    df = df.copy()
    le = LabelEncoder()
    for k, v in encoding_dict.items():
        try:
            le_class = v
            fit_le = le.fit(le_class)
            df[k] = fit_le.transform(df[k])
        except:
            continue
    return df


def model_and_encoding_loader_script(df: DataFrame, model) -> tuple:
    """
    get tuple of model and categorical encoding dictionary
    :param df: the pre-processed training data
    :param model: the model
    :return: tuple of the model and categorical encoding dictionary
    """
    df = df.copy()
    cat_encode_dict = categorical_encoding_loader_scrip(df)

    df = encode_categorical_fields(df, cat_encode_dict)
    X_train, X_test, y_train, y_test = divide_train_test(df, target=TARGET)
    model.fit(X_train, y_train)

    # persist the model for future use
    return model, cat_encode_dict


def train_model(prep_train_data_dict: dict) -> tuple:
    """
    dictionary of model, categorical encodings
    :param prep_train_data_dict: the pre-processed training data dictionary
    :return: dictionary of the model, categorical encodings
    """
    model_out_dict = {}
    cat_encode_out_dict = {}

    for prep_data_dict_key, prep_data_dict_value in prep_train_data_dict.items():
        model_dict = model_loader_script()
        model = model_dict[prep_data_dict_key]
        model_out_dict[prep_data_dict_key], cat_encode_out_dict[prep_data_dict_key] = model_and_encoding_loader_script(prep_data_dict_value, model)

    return model_out_dict, cat_encode_out_dict
