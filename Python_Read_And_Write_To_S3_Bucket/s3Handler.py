import boto3
import configparser
import pandas as pd
from io import BytesIO

config = configparser.ConfigParser()
config.read('config.ini')

access_key = config.get('AWS', 'access_key')
secret_key = config.get('AWS', 'secret_key')

class S3Handler:
    """
    A class for handling s3 operations such as reading and writing to s3 bucket
    """

    def __init__(self, bucket_name: str, bucket_folder: str):
        """
        :param bucket_name: The name of the s3 bucket to read from and write to.
        :param bucket_folder: The folder name in the bucket to write the file to or read from.
        """
        self.bucket_name = bucket_name
        self.bucket_folder = bucket_folder
        self.s3_resource = boto3.resource('s3', aws_access_key_id=access_key, aws_secret_access_key=secret_key)

    def _set_s3_folder_where_necessary(self, file_key: str) -> str:
        """
        Set the name of the file
        :param file_key: The key or name of the file to read from or write to on the s3 bucket.
        """
        return self.bucket_folder + '/' + file_key if self.bucket_folder is not None else file_key

    def list_s3_files(self) -> list:
        """
        List the files on the s3 bucket
        :return: The list of file key(s) or name(s) of the file available on the s3 bucket.
        """
        obj = self.s3_resource.Bucket(self.bucket_name).objects.all()
        return [val.key for val in obj]

    def write_to_s3(self, df: pd.DataFrame, file_key: str):
        """
        Write parquet file to s3 bucket
        :param df: data as a pandas DataFrame.
        :param file_key: The key or name of the file to write to on the s3 bucket.
        """
        buffer = BytesIO()
        df.to_parquet(buffer, index=False)
        s3_key = self._set_s3_folder_where_necessary(file_key)
        obj = self.s3_resource.Object(self.bucket_name, s3_key)
        response = obj.put(Body=buffer.getvalue())
        # if response['ResponseMetadata'][HTTPStatusCode] ==  200:
        #     f'{file_key} data successfully loaded.'

    def read_from_s3(self, file_key: str) -> pd.DataFrame:
        """
        Write parquet file to s3 bucket
        :param file_key: The key or name of the file to read from the se bucket.
        :return: Dataset in the form of pandas DataFrame.
        """
        s3_key = self._set_s3_folder_where_necessary(file_key)
        obj = self.s3_resource.Object(self.bucket_name, s3_key)
        response = obj.get()
        file_content = response['Body'].read()

        return pd.read_parquet(BytesIO(file_content))
