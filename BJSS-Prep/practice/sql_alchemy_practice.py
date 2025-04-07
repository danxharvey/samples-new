from sqlalchemy import create_engine, text
import pandas as pd


# Create in memory SQL connection
engine = create_engine('sqlite://', echo=False)

# Create simple table with 3 users
df = pd.DataFrame({'name': ['User1', 'User2', 'User3']})
df1 = pd.DataFrame({'name': ['User4', 'User5']})
df2 = pd.DataFrame({'name': ['User4', 'User5', 'User6', 'User7']})

# Write to SQL and read back
df.to_sql(name='tbl_users', con=engine, if_exists='replace', index_label='id')
with engine.connect() as conn:
    df_read1 = conn.execute(text('SELECT * FROM tbl_users')).fetchall()
print(df_read1)

# Write to SQL with append
df1.to_sql(name='tbl_users', con=engine, if_exists='append', index_label='id')
with engine.connect() as conn:
    df_read2 = conn.execute(text('SELECT * FROM tbl_users')).fetchall()
print(df_read2)

# Write to SQL with replace
df2.to_sql(name='tbl_users', con=engine, if_exists='replace', index_label='id')
with engine.connect() as conn:
    df_read3 = conn.execute(text('SELECT * FROM tbl_users')).fetchall()
print(df_read3)
