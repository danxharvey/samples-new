from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from yaml import safe_load
from json import load
from sys import exit
import pandas as pd
import logging

class LogWriter(object):
    def __init__(self):
        # Initialise logging
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(logging.INFO)
        FORMAT = '%(asctime)s - %(levelname)s - PIPELINE ACTIVITIES => %(message)s'
        self.formatter = logging.Formatter(FORMAT)
        self.handler = logging.FileHandler('logs/pipeline.log', mode='w')
        self.handler.setFormatter(self.formatter)
        self.logger.addHandler(self.handler)
                

class DataPipeline():
    ''' Class for data pipeline object '''
    def __init__(self, arg_dict: dict):
        self.source: str = arg_dict['src']
        self.target: str = arg_dict['dest']
        self.plots: str = arg_dict['plots']
        self.conv_dtype: dict = arg_dict['dconv']
        self.col_renames: dict = arg_dict['cnames']
        self.viz_type: str = arg_dict['viz']
        self.df: pd.DataFrame = pd.DataFrame
        logger.logger.info(f'Data pipeline object created')
        print('\tData pipeline object created')
        
    def data_load(self):
        ''' Loads data '''
        self.df = pd.read_csv(self.source)
        logger.logger.info(f'Data loaded')
        print('\tData loaded')

    def data_clean(self):
        ''' Basic clean of data once loaded '''
        self.df = (self.df
                   .drop_duplicates()
                   .dropna()
                   .reset_index(drop=True))
        logger.logger.info(f'Data cleaned')
        print('\tData cleaned')

    def data_dtypes_rename(self):
        ''' Performs some dynamic updates '''
        # Fully aware more validation/conditions could be applied. Demo purpose only.
        self.df = (self.df
                   .astype(dtype=self.conv_dtype)
                   .rename(columns=self.col_renames))
        self.df['Date'] = pd.to_datetime(self.df['Date'])
        self.df['Month'] = self.df['Date'].dt.month
        logger.logger.info(f'Datatypes converted and columns renamed')
        print('\tDatatypes converted and columns renamed')

    def data_summarise(self):
        ''' Creates a simple summary '''
        self.df = self.df.groupby('Month')['Units Sold'].mean()
        logger.logger.info(f'Data summarised')
        print('\tData summarised')
        
    def data_viz(self):
        ''' Plots a chart '''
        viz = self.df.plot(kind=self.viz_type, figsize=(10,5), title='Avg Units Sold per Month')
        viz.figure.savefig(self.plots)
        logger.logger.info(f'Data viz saved')
        print(f'\tData viz saved to {self.plots}')

    def data_write(self):
        ''' Saves chart data '''
        self.df.to_csv(self.target)
        logger.logger.info(f'Data output saved')
        print(f'\tData output saved to {self.target}')
        
    def exec_pipeline(self):
        ''' Runs pipeline '''
        # Fully aware more error conditions could be handled here. Demo purposes only
        self.data_load()
        self.data_clean()
        self.data_dtypes_rename()
        self.data_summarise()
        self.data_viz()
        self.data_write()
        logger.logger.info(f'Data pipeline completed')
        print('\nSUCCESS! Data pipeline completed\n\n')


def arg_pars() -> dict:
    ''' Function to parse args from the CLI ''' 
    # Set arg parser for using yaml or json config options
    parser = ArgumentParser(description='Enter desired config file for the pipeline', 
                        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('conf', help='Use "config.json" OR "config.yaml". Type: String', type=str)
    pars = vars(parser.parse_args())
    conf = pars['conf']
    print('\nChecking for valid config filename...')
    try:
        print('\tConfig accepted!') if conf in ['config.json', 'config.yaml'] else exit()
    except SystemExit:
        print('\tConfig rejected! You must enter "config.json" or "config.yaml"')
        logger.logger.error(f'Invalid config filename entered')
        exit()
    return conf


def load_configs(conf: str) -> dict:
    ''' Function to load config variables for data pipeline ''' 
    # Load config from selected file
    with open(conf, "r") as file:
        if conf == 'config.yaml':
            args = safe_load(file)
        elif conf == 'config.json':
            args = load(file)
    return args


# Run only if this is calling module
if __name__ == '__main__':

    # Initialise logger
    logger = LogWriter()

    # Initialise arg parser and check config file is valid
    conf = arg_pars()
    
    # Load user defined config file
    print(f'\nLoading variables from {conf}...')
    args = load_configs(conf)
    print('\tVariables loaded')

    # Run pipeline and cleanup for good manners
    print('\nStarting pipeline...')
    print('\tCreating data pipeline object')
    pipeline = DataPipeline(args['var_dict'])
    print('\tExecuting pipeline code')
    pipeline.exec_pipeline()
    del conf, args, pipeline