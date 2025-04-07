import pandas as pd

def load_csv(f):
    pass


def load_json(f):
    pass



# # Dataframe processing helper functions
# def get_no_damage(df):
#     """ Replace NaN values in data frame condition column """
#     df['label_name'] = df['label_name'].fillna('No Damage').reset_index(drop=True)
#     return df


# def remove_bad_coords(df):
#     """
#     Remove samples when the height or width of the bounding box is zero or na
#     This is a potential data annotation issue with the bounding box coords
#     """
#     df = df[df.reset_index(drop=True)['bbox_x'].notna()]
#     df = df[df.reset_index(drop=True)['bbox_y'].notna()]
#     df = df[df['bbox_width'] != 0]
#     df = df[df['bbox_height'] != 0]
#     return df


# def make_new_columns(df):
#     """ Make new columns for dataset creation """
#     df[c.target] = df['label_name'].str.lower().map(c.damage_dict)
#     df['x_min'] = df['bbox_x']
#     df['x_max'] = df['bbox_x'] + df['bbox_width']
#     df['y_min'] = df['bbox_y']
#     df['y_max'] = df['bbox_y'] + df['bbox_height']
#     # df['path'] = c.datadir + 'images/' + df['image_name']
#     df['path'] = c.datadir + 'images/' + df['image_name']
#     df.dropna()
#     return df[['damage_id', 'x_min', 'x_max', 'y_min', 'y_max', 'image_width', 'image_height', 'image_name', 'path']]


# def match_annotations_to_images(df):
#     """ Ensure only annotations with matching images in the image folder are used """
#     imgs = [f for f in os.listdir(c.datadir + 'images/') if os.path.isfile(os.path.join(c.datadir + 'images/', f))]
#     # imgs = [f for f in os.listdir(c.datadir + 'images/no_damage/') if os.path.isfile(os.path.join(c.datadir + 'images/no_damage/', f))]
#     return df.loc[df['image_name'].isin(imgs)].reset_index(drop=True)


# def prepare_df(df):
#     """ Clean and prepare data frame """
#     df = get_no_damage(df)
#     df = remove_bad_coords(df)
#     df = make_new_columns(df)
#     df = match_annotations_to_images(df)
#     return df.sample(frac=1).reset_index(drop=True)



# # Load csv data
# df = pd.read_csv(c.datadir + "labels/labels_p32_risse2_schatten_lachs.csv")
# df_damage = prepare_df(df)
# print('Check only 1 image shape present:\nScan image_width values...\t{}\
#       \nScan image_height values...\t{}'.format(df_damage['image_width'].unique(), df_damage['image_height'].unique()))

# # View summary
# imgs = [f for f in os.listdir(c.datadir + 'images/') if os.path.isfile(os.path.join(c.datadir + 'images/', f))]
# print('\nTotal rows in dataset: {}\nUnique images in dataset: {}\nUnique images in images folder {}\n'
#     .format(df_damage.shape, df_damage['image_name'].nunique(), len(imgs)))

# # View summary
# display(df_damage.head(5))





# # # Get unique height and width from damage dataframe
# # w, h = df_damage['image_width'].unique(), df_damage['image_height'].unique()

# # # Get list of no damage images
# # imgs = [f for f in os.listdir(c.nodamdir) if os.path.isfile(os.path.join(c.nodamdir, f))]

# # # Assert same proportions on no damage images
# # for img in imgs:
# #     i = cv2.imread(c.nodamdir + img, 0)
# #     assert i.shape[0] == h
# #     assert i.shape[1] == w
# #     del i

# # # Create dataframe
# # df_no_damage = pd.DataFrame(imgs, columns=['img'])
# # df_no_damage['damage_id'] = 0
# # df_no_damage['x_min'] = 0
# # df_no_damage['x_max'] = 224
# # df_no_damage['y_min'] = 0
# # df_no_damage['y_max'] = 224
# # df_no_damage['image_width'] = int(w)
# # df_no_damage['image_height'] = int(h)
# # df_no_damage['image_name'] = df_no_damage['img']
# # df_no_damage['path'] = c.nodamdir + df_no_damage['image_name']
# # df_no_damage = df_no_damage.drop('img', axis=1)
# # df_no_damage.head()




# # Load dataset
# df_bg_feat = pd.read_csv(c.datadir + 'labels/labels_p3_no_damage.csv')
# # Get unique image names and check they exist already
# files = np.unique([f for f in df_bg_feat['image_name']])
# imgs = [f for f in os.listdir(c.nodamdir) if os.path.isfile(os.path.join(c.nodamdir, f))]
# matches = set(files).intersection(imgs)
# assert len(files) == len(matches)

# # Process dataset
# df_bg_feat = prepare_df(df_bg_feat)
# df_bg_feat.head(5)




# # Merge dataframes and reset index
# #print(len(df_damage))
# print(len(df_bg_feat))
# df_merged = pd.concat([df_damage, df_bg_feat]).reset_index(drop=True)
# #df_merged = pd.concat([df_damage, df_no_damage]).reset_index(drop=True)
# print(len(df_merged))

# # Clean up
# del df_damage, df_bg_feat
# # del df_damage, df_no_damage




