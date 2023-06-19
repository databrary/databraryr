# Package requirements

## Authentication/security

- Enable password-protected access to Databrary via API.
  - config_passwd()
  - login_db()
  - logout_db()

## Downloads

- Download specific files
- Videos
- CSV spreadsheets
    - read_csv_data_as_df()
    - download_session_csv() # how differ from read_csv_data_as_df()?
- Session (folders) as .zip
- Volume as .zip

## Get/list info 

- from/about Databrary
    - get_db_stats()
- from/about a party (person or institution)
    - is_institution()
    - list_sponsors()
        - list_individual_sponsors()
        - list_institutional_sponsors()
    - list_affiliates()
- from/about a volume
    - list_volume() # basic info, check overlap with list_containers_records()
    - list_volume_metadata() # vol_id, name, owners, permission/access, DOI
    - list_volume_owners()
    - list_volume_tags()
    - list_containers_records(), list_containers_records_json()
    - list_assets_by_type()
    - list_assets_in_volume() # how differ from list_asset_by_type()?
    - list_volume_activity() # history of modifications
    - list_volume_exerpts()
    - list_volume_funding()
    - list_volume_links()
    - list_volume_funding()
    - summarize_demo_part_w_video() # Demographic info about sessions w/ videos
    - summarize_videos_in_volume()
    - get_video_stats() # rename to get_vol_video_stats()?
- from/about a session
    - list_assets_in_session()
- from/about files
    - get_file_duration()
    - get_asset_segment_range()
- about Databrary-wide constants
    - assign_constants()
    - permission levels via get_permission_levels()) # from participants
    - release levels via get_release_levels() # assigned by Databrary
    - file types: get_supported_file_types()

## Search

- For funder info
    - search_for_funder()
- For keywords & tags
    - search_for_keywords()
    - search_for_tags()

## Uploads (not implemented yet)


---

## Other clean-up

### Remove deprecated functions

- download_containers_records()
- download_party_json(); duplicated functionality in download_party()
- Compare download_video() to download_asset()

### Consider creating utils.R

- format_to_filetypes()
- get_asset_segment_range()
- HHMMSSmmm_to_ms()
- is_institution()

### Consider creating separate datavyr package

- Example is dv_to_csv(), extract_dv_code_defs(), extract_dv()

### Expand tests

- download_video()

### Improve function documentation

- Add better examples

