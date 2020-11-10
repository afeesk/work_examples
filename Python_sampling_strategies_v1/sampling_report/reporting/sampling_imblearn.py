from pandas import DataFrame
import numpy as np
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.svm import SVC, LinearSVC, NuSVC
from sklearn.naive_bayes import MultinomialNB, GaussianNB, BernoulliNB, ComplementNB
from sklearn.preprocessing import LabelEncoder, OrdinalEncoder
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score, f1_score
from imblearn.under_sampling import (TomekLinks, ClusterCentroids, CondensedNearestNeighbour, EditedNearestNeighbours,
                                     RepeatedEditedNearestNeighbours, AllKNN, InstanceHardnessThreshold, NearMiss,
                                     NeighbourhoodCleaningRule, OneSidedSelection, RandomUnderSampler)
from imblearn.over_sampling import ADASYN, BorderlineSMOTE, KMeansSMOTE, RandomOverSampler, SMOTE, SVMSMOTE
from imblearn.combine import SMOTEENN, SMOTETomek
import warnings

warnings.filterwarnings('ignore')

RANDOM_STATE = 0


def categorical_encoding_loader_script(df: DataFrame) -> dict:
    """
    :param df: The dataset in the form of pandas DataFrame.
    :return: dictionary of the ordinal encodings of the categorical features.
    """
    df = df.copy()
    encoding_dict = {}
    cat_var_cols = df.select_dtypes(include=['object']).columns
    ordinal_enc = OrdinalEncoder(dtype=np.int64)
    df[cat_var_cols] = ordinal_enc.fit_transform(df[cat_var_cols])
    for i, cat in enumerate(df[cat_var_cols].columns):
        encodings = ordinal_enc.categories_[i]
        encoding_dict[cat] = encodings
    return encoding_dict


def encode_categorical_field(df: DataFrame, encoding_dict: dict) -> DataFrame:
    """
    :param df: The dataset in the form of pandas DataFrame.
    :param encoding_dict: ordinal encodings of the categorical features.
    :return: The dataset after encoding.
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


def get_cv_result(X, y, model, random_state, class_weight) -> tuple:
    """
    :param X: X_Sampling from the sampling strategy.
    :param y: y_sampling
                   The class variable must be one of the columns in the data
    :param model: Name of the model. Can be one or combination of:
                  ['rf', 'tree', 'lg', 'gb', 'knn', 'nn', 'svm', 'linear_svm', 'nu_svm', 'nb', 'gnb', 'bnb', 'cnb']
                  Notes: String or list is expected for single model. List is expected for multiple models.
    :param random_state: int or RandomState instance, default=None
                         Controls the randomness of the training and testing indices produced.
                         Pass an int for reproducible output across multiple function calls
    :param class_weight:  ‘balanced’ or 'balanced_subsample', default=None
    :return:  F1_macro, Accuracy and Model name.
    """
    cv = []
    cv_f1 = []
    skf = StratifiedKFold(n_splits=10, shuffle=True, random_state=random_state)
    for train_index, test_index in skf.split(X, y):
        X_train, X_test = X.iloc[train_index], X.iloc[test_index]
        y_train, y_test = y[train_index], y[test_index]
        if model == 'rf':
            name = RandomForestClassifier
            model = RandomForestClassifier(n_jobs=-1, random_state=random_state, class_weight=class_weight)
        if model == 'tree':
            name = DecisionTreeClassifier
            model = DecisionTreeClassifier(random_state=random_state, class_weight=class_weight)
        if model == 'lg':
            name = LogisticRegression
            model = LogisticRegression(random_state=random_state, class_weight=class_weight)
        if model == 'gb':
            name = GradientBoostingClassifier
            model = GradientBoostingClassifier(random_state=random_state)
        if model == 'knn':
            name = KNeighborsClassifier
            model = KNeighborsClassifier()
        if model == 'nn':
            name = MLPClassifier
            model = MLPClassifier(random_state=random_state)
        if model == 'svm':
            name = SVC
            model = SVC(random_state=random_state, class_weight=class_weight)
        if model == 'linear_svm':
            name = LinearSVC
            model = LinearSVC(random_state=random_state, class_weight=class_weight)
        if model == 'nu_svm':
            name = NuSVC
            model = NuSVC(random_state=random_state, class_weight=class_weight)
        if model == 'nb':
            name = MultinomialNB
            model = MultinomialNB()
        if model == 'gnb':
            name = GaussianNB
            model = GaussianNB()
        if model == 'bnb':
            name = BernoulliNB
            model = BernoulliNB()
        if model == 'cnb':
            name = ComplementNB
            model = ComplementNB()
        model.fit(X_train, y_train)
        pred = model.predict(X_test)
        acc = accuracy_score(y_true=y_test, y_pred=pred)
        cv.append(acc)
        f1 = f1_score(y_true=y_test, y_pred=pred, average='macro')
        cv_f1.append(f1)
    cv_avg = round(np.mean(cv), 3)
    cv_f1_avg = round(np.mean(cv_f1), 3)

    return cv_f1_avg, cv_avg, name.__name__


def sampling_method(method, df: DataFrame, target: str, sampling_strategy) -> tuple:
    """

    :param method: All methods are preset.
                   Note: Methods used were implemented in the imbalanced-learn API.
    :param df: Dataset including features and target.
                 Notes: Dataset in the form of Pandas DataFrame.
                 Dataset is expected to be preprocessed with no missing values.
                 sampling_report can automatically handle categorical encoding.
    :param target: str
                   The class variable must be one of the columns in the data
    :param sampling_strategy: All methods are preset.
                              Note: sampling_strategy used were implemented in the imbalanced-learn API.
    :return: resampled features, target and target encodings.
    """
    le = LabelEncoder()
    under_no_seed_version = [NearMiss]
    under_no_seed = [EditedNearestNeighbours, RepeatedEditedNearestNeighbours, AllKNN,
                     NeighbourhoodCleaningRule, TomekLinks]
    under_with_seed = [ClusterCentroids, CondensedNearestNeighbour, InstanceHardnessThreshold,
                       OneSidedSelection, RandomUnderSampler]
    over_with_seed = [ADASYN, BorderlineSMOTE, KMeansSMOTE, RandomOverSampler, SMOTE, SVMSMOTE]
    combine_with_seed = [SMOTEENN, SMOTETomek]

    if method in under_no_seed_version:
        sampling_method_use = method(sampling_strategy=sampling_strategy, version=2)

    if method in under_no_seed:
        sampling_method_use = method(sampling_strategy=sampling_strategy)

    if method in under_with_seed:
        sampling_method_use = method(sampling_strategy=sampling_strategy, random_state=RANDOM_STATE)

    if method in over_with_seed:
        sampling_method_use = method(sampling_strategy=sampling_strategy, random_state=RANDOM_STATE)

    if method in combine_with_seed:
        sampling_method_use = method(sampling_strategy=sampling_strategy, random_state=RANDOM_STATE)
    df = df.copy()
    le.fit(df[target])
    class_encoder = le.classes_
    df = encode_categorical_field(df, categorical_encoding_loader_script(df))
    y = df[target]
    X = df.drop([target], axis=1)
    X_sampling, y_sampling = sampling_method_use.fit_sample(X, y)

    return X_sampling, y_sampling, class_encoder


def get_sampling_reports(df: DataFrame, target: str, method, model,
                         random_state, class_weight):
    """
    :param df: Dataset including features and target.
                 Notes: Dataset in the form of Pandas DataFrame.
                 Dataset is expected to be preprocessed with no missing values.
                 sampling_report can automatically handle categorical encoding.
    :param target: str
                   The class variable must be one of the columns in the data
    :param method: All methods are preset.
                   Note: Methods used were implemented in the imbalanced-learn API.
    :param model: Name of the model. Can be one or combination of:
                  ['rf', 'tree', 'lg', 'gb', 'knn', 'nn', 'svm', 'linear_svm', 'nu_svm', 'nb', 'gnb', 'bnb', 'cnb']
                  Notes: String or list is expected for single model. List is expected for multiple models.
    :param random_state: int or RandomState instance, default=None
                         Controls the randomness of the training and testing indices produced.
                         Pass an int for reproducible output across multiple function calls
    :param class_weight:  ‘balanced’ or 'balanced_subsample', default=None
    :return:  F1_macro, Accuracy, Model name and Sampling strategy.
    """
    le = LabelEncoder()
    sampling_strategy_under = ['all', 'not minority', 'majority', 'not majority']
    sampling_strategy_over_combine = ['all', 'not majority', 'minority', 'not minority']
    under_sampling = [NearMiss, EditedNearestNeighbours, RepeatedEditedNearestNeighbours, AllKNN,
                      NeighbourhoodCleaningRule, TomekLinks, ClusterCentroids, CondensedNearestNeighbour,
                      InstanceHardnessThreshold, OneSidedSelection, RandomUnderSampler]
    over_sampling = [ADASYN, BorderlineSMOTE, KMeansSMOTE, RandomOverSampler, SMOTE, SVMSMOTE]
    combine = [SMOTEENN, SMOTETomek]

    if method in under_sampling:
        for strategy in sampling_strategy_under:
            try:
                X_sampling, y_sampling, class_encoder = sampling_method(method, df, target, strategy)
            except:
                print('Sampling method not suitable for the dataset')
                continue
            le.fit(class_encoder)
            unique, counts = np.unique(np.array(y_sampling), return_counts=True)
            unique = le.inverse_transform(unique)
            class_counter = dict(zip(unique, counts))
            print(f'Class distribution after sampling: {class_counter}')
            try:
                cv_f1_avg, cv_avg, name = get_cv_result(X_sampling, y_sampling, model, random_state, class_weight)
            except:
                print('Sampling method left not enough classes for the classifier to learn')
                continue
            print(f'mean_f1: {cv_f1_avg} | mean_acc: {cv_avg}  | model_name: {name} | sampling_strategy: {strategy}')
            print()
    if method in over_sampling or method in combine:
        for strategy in sampling_strategy_over_combine:
            try:
                X_sampling, y_sampling, class_encoder = sampling_method(method, df, target, strategy)
            except:
                print('Sampling method not suitable for the dataset')
                continue
            le.fit(class_encoder)
            unique, counts = np.unique(np.array(y_sampling), return_counts=True)
            unique = le.inverse_transform(unique)
            class_counter = dict(zip(unique, counts))
            print(f'Class distribution after sampling: {class_counter}')
            try:
                cv_f1_avg, cv_avg, name = get_cv_result(X_sampling, y_sampling, model, random_state, class_weight)
            except:
                print('Sampling method left not enough classes for the classifier to learn')
                continue
            print(f'mean_f1: {cv_f1_avg} | mean_acc: {cv_avg}  | model_name: {name} | sampling_strategy: {strategy}')
            print()


def sampling_report(data: DataFrame, target: str, model, random_state=None,
                    class_weight=None, exclude=None, include=None):
    """
    :param data: Dataset including features and target.
                 Notes: Dataset in the form of Pandas DataFrame.
                 Dataset is expected to be preprocessed with no missing values.
                 sampling_report can automatically handle categorical encoding.
    :param target: str
                   The class variable must be one of the columns in the data
    :param model: Name of the model. Can be one or combination of:
                  ['rf', 'tree', 'lg', 'gb', 'knn', 'nn', 'svm', 'linear_svm', 'nu_svm', 'nb', 'gnb', 'bnb', 'cnb']
                  Notes: String or list is expected for single model. List is expected for multiple models.
    :param random_state: int or RandomState instance, default=None
                         Controls the randomness of the training and testing indices produced.
                         Pass an int for reproducible output across multiple function calls
    :param class_weight:  ‘balanced’ or 'balanced_subsample', default=None
    :param include: str or list
                    To include one or more of the sampling methods below:
                    'EditedNearestNeighbours', 'RepeatedEditedNearestNeighbours', 'AllKNN',
                   'NearMiss', 'NeighbourhoodCleaningRule', 'TomekLinks', 'ClusterCentroids',
                   'CondensedNearestNeighbour', 'InstanceHardnessThreshold', 'OneSidedSelection',
                   'RandomUnderSampler', 'ADASYN', 'BorderlineSMOTE', 'KMeansSMOTE', 'RandomOverSampler',
                   'SMOTE', 'SVMSMOTE', 'SMOTEENN', 'SMOTETomek'
    :param exclude: str or list
                    To exclude one or more of the sampling methods below:
                    'EditedNearestNeighbours', 'RepeatedEditedNearestNeighbours', 'AllKNN',
                   'NearMiss', 'NeighbourhoodCleaningRule', 'TomekLinks', 'ClusterCentroids',
                   'CondensedNearestNeighbour', 'InstanceHardnessThreshold', 'OneSidedSelection',
                   'RandomUnderSampler', 'ADASYN', 'BorderlineSMOTE', 'KMeansSMOTE', 'RandomOverSampler',
                   'SMOTE', 'SVMSMOTE', 'SMOTEENN', 'SMOTETomek'
    :return: Sampling method, Class distribution after sampling, F1_macro, Accuracy,
             Model name and Sampling strategy.
    """
    if target not in data.columns:
        raise ValueError(f'None of the columns contain "{target}".')
    models = ['rf', 'tree', 'lg', 'gb', 'knn', 'nn', 'svm', 'linear_svm', 'nu_svm', 'nb', 'gnb', 'bnb', 'cnb']
    check_list = isinstance(model, list)
    if check_list is False:
        model = [model]
    check = all(value in models for value in model)
    if check is False:
        raise ValueError(f'Valid presets for model include {models}. Given "{model}"')

    if class_weight is not None:
        class_weight = class_weight
        if class_weight not in ['balanced', 'balanced_subsample']:
            raise ValueError(
                f'Valid presets for class_weight include "balanced" and "balanced_subsample".Given "{class_weight}"')
    if random_state is not None:
        random_state = random_state

    all_methods = ['EditedNearestNeighbours', 'RepeatedEditedNearestNeighbours', 'AllKNN',
                   'NearMiss', 'NeighbourhoodCleaningRule', 'TomekLinks', 'ClusterCentroids',
                   'CondensedNearestNeighbour', 'InstanceHardnessThreshold', 'OneSidedSelection',
                   'RandomUnderSampler', 'ADASYN', 'BorderlineSMOTE', 'KMeansSMOTE', 'RandomOverSampler',
                   'SMOTE', 'SVMSMOTE', 'SMOTEENN', 'SMOTETomek']
    if exclude is not None and include is not None:
        raise ValueError(f'Please specify one of include or exclude')

    if exclude is not None:
        check_list_exclude = isinstance(exclude, list)
        if check_list_exclude is False:
            exclude = [exclude]
        check_exclude = all(value in all_methods for value in exclude)
        if check_exclude is False:
            raise ValueError(f'Valid presets for model include {all_methods}. Given "{exclude}"')
        else:
            [all_methods.remove(ex) for ex in exclude]
            print(f'excluding {exclude} from the sampling methods')

    if include is not None:
        check_list_include = isinstance(include, list)
        if check_list_include is False:
            include = [include]
        check_include = all(value in all_methods for value in include)
        if check_include is False:
            raise ValueError(f'Valid presets for model include {all_methods}. Given "{include}"')
        else:
            all_methods = include
            print(f'sampling methods for {include}')

    for m in model:
        for method in all_methods:
            method_e = eval(method)
            print()
            print(method)
            get_sampling_reports(df=data, target=target, method=method_e, model=m,
                                 random_state=random_state, class_weight=class_weight)
