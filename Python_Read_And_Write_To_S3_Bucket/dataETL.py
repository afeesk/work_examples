import os
import csv
import logging
import pandas as pd
import numpy as np

from s3Handler import S3Handler
from config.loader_config import *

logging.basicConfig(format=' %(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class DataETL:
    """
    A class for extracting data from a local folder and loading into s3 target.
    """

    def __init__(self, bucket_name, bucket_folder=None, source_path=None):
        """
        :param bucket_name: The name of the s3 bucket to read from or write to.
        :param bucket_folder: str, None
                              The folder name in the bucket to write the file to or read from.
                              Default is None
        : param source_path: str, None
                             The path to the local folder to extract data from.
                             Default is None
        """
        self.bucket_name = bucket_name
        self.bucket_folder = bucket_folder
        self.source_path = source_path

        self.s3_connection = S3Handler(bucket_name=self.bucket_name, bucket_folder=self.bucket_folder)

    def _validate_and_get_file_name(self, path: str) -> list:
        """
        To get filename and ensure that folder is not empty.
        :param path: The path to the required dataset(s) on the local folder.
        :return: List of all files in the path.
        """
        files = os.listdir(path)
        if len(files) < 1:
            raise FileNotFoundError(f'No file detected in {self.path}')
        if path is None:
            raise ValueError(f'File path must be supplied.')

        return files

    def _get_delimiter(self, path: str) -> str:
        """
        To get the delimiter used to separate the data content, useful for csv and txt file formats.
        :param path: The path to the csv file
        return: The delimiter as string
        """
        with open(path, 'r') as f:
            first_line = f.readline()
            dialect = csv.Sniffer().sniff(first_line)

            return dialect.delimiter

    def _read_data(self, file: str, file_extension: str) -> pd.DataFrame:
        """
        Read data.
        :param file: name of file.
        :param file_extension: extension of the file.
        :return: Dataset in the form of pandas DataFrame.
        """
        if file_extension == 'csv':
            data = pd.read_csv(os.path.join(self.source_path, file),
            sep=self._get_delimiter(os.path.join(self.source_path, file)))

        if file_extension == 'parquet':
            data = pd.read_parquet(os.path.join(self.source_path, file))

        return data

    def _extract_data(self) -> dict:
        """
        Extract data from local directory.
        :return: dictionary of the dataset containing the filename as key and dataFrame as value.
        """
        df_dict = {}
        files = self._validate_and_get_file_name(self.source_path)
        for file in files:
            f = file.split('.')
            if len(f) > 2:
                filename = ['.'.join(obj for obj in f[:-1])][0]
            else:
                filename = f[0]
            file_extension = f[-1]
            if file_extension not in SUPPORTED_FILE_EXTENSION:
                logger.warning(f"Skipping {file}, we don't currently support {file_extension}")
                continue
            logger.info(f'Extracting {filename} data...')
            df = self._read_data(file, file_extension)
            df_dict[filename] = df
        logger.info(f'Extraction completed')
        return df_dict

    def load_data_to_s3(self):
        """
        Save the locally extracted data to the s3 bucket.
        """
        df_dict = self._extract_data()
        for k, df in df_dict.items():
            logger.info(f'Loading {k} data to s3...')
            self.s3_connection.write_to_s3(df=df, file_key=k + '.parquet')
        logger.info(f'Loading completed')

    def extract_data_from_s3(self, file_key: str) -> pd.DataFrame:
        """
        Extract data from an s3 bucket.
        :return: The dataset as a pandas dataFrame.
        """
        logger.info(f'Extracting {file_key} data from s3...')
        data = self.s3_connection.read_from_s3(file_key)
        logger.info(f'Extraction completed')
        return data

    def list_s3_files(self):
        """
        List the files on the s3 bucket.
        :return: The list of file key(s) of the files and folders available on the s3 bucket.
        """

        return self.s3_connection.list_s3_files()
