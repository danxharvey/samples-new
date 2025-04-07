import json
import pandas as pd
from dataclasses import dataclass
from datetime import datetime
from time import time
# from pydantic import BaseModel

@dataclass
# class Driver(BaseModel):  # Removes need for __post_init__
class Driver:
    driverId: int
    driverRef: str
    number: int
    code: str
    forename: str
    surname: str
    dob: datetime
    nationality: str
    url: str

    def __post_init__(self):
        # Validate for type checks as pydantic needs python-open-cv upgrade
        if not isinstance(self.driverId, int):
            raise TypeError(f'Expected int for driverId, got {type(self.driverId).__name__}')
        if not isinstance(self.driverRef, str):
            raise TypeError(f'Expected str for driverRef, got {type(self.driverRef).__name__}')
        if not isinstance(self.number, int):
            raise TypeError(f'Expected int for number, got {type(self.number).__name__}')
        if not isinstance(self.code, str):
            raise TypeError(f'Expected str for code, got {type(self.code).__name__}')
        if not isinstance(self.forename, str):
            raise TypeError(f'Expected str for forename, got {type(self.forename).__name__}')
        if not isinstance(self.surname, str):
            raise TypeError(f'Expected str for lastname, got {type(self.surname).__name__}')
        if not isinstance(self.dob, str):
            raise TypeError(f'Expected datetime for dob, got {type(self.dob).__name__}')
        if not isinstance(self.nationality, str):
            raise TypeError(f'Expected str for nationality, got {type(self.nationality).__name__}')
        if not isinstance(self.url, str):
            raise TypeError(f'Expected str for url, got {type(self.url).__name__}')


def load_json_to_dataclass(json_string):
    data = json.loads(json_string)

    # Split the 'name' into forename and last name
    if 'name' in data:
        data['forename'] = data['name']['forename']
        data['surname'] = data['name']['surname']
        data.pop('name', None)
    return Driver(**data)


if __name__ == '__main__':

    start = time()
    # Initialise dictionary to fill and counters
    driver_dict, rec_count, uid = {}, 0, 0

    # Parse file by line and store in dict for speed
    with open('drivers.json') as f:
        for line in f:
            try:
                driver = load_json_to_dataclass(line)
                driver_dict[uid] = driver
                uid += 1
                del driver
                # print('Record loaded successfully', driver, '\n')
            except Exception as e:
                # print('Error loading record:', e, '\n')
                pass
            finally:
                rec_count += 1

    # Check load stats            
    print(f'\nNumber of rows loaded: {len(driver_dict)}')
    print(f'Number of rows in file: {rec_count}\n')

    # Convert parsed dict to pandas dataframe and drop url column
    final = pd.DataFrame.from_dict(driver_dict, orient='index').drop(['url'], axis=1)
    print(final.head(5))

    # Write output to csv and check
    final.to_csv('drivers_df_output.csv', index=False)
    end = time()
    print(f'Elapsed time: {end-start}')