
import os
import requests
import pandas as pd
import time
import json
from typing import List, Dict, Any

# --- CONFIG ---
RIOT_API_KEY = os.getenv('RIOT_API_KEY') or os.getenv('api_key')
REGION_HOST = 'europe.api.riotgames.com'

# --- HELPERS ---
def riot_get_json(url: str, max_retries: int = 5, sleep_base: float = 1.0) -> Any:
    headers = {'X-Riot-Token': RIOT_API_KEY}
    pause = sleep_base
    for attempt in range(max_retries):
        resp = requests.get(url, headers=headers)
        if resp.status_code == 200:
            return resp.json()
        if resp.status_code in {429, 500, 502, 503, 504}:
            print(f"[{resp.status_code}] {url} -> retry in {pause}s")
            time.sleep(pause)
            pause = min(pause * 2, 16)
            continue
        print(f"Non-retryable {resp.status_code} for {url}")
        return None
    print(f"Max retries exceeded: {url}")
    return None

def get_puuid(summoner_name: str, tag: str) -> str:
    url = f"https://{REGION_HOST}/riot/account/v1/accounts/by-riot-id/{summoner_name}/{tag}"
    data = riot_get_json(url)
    if data and 'puuid' in data:
        return data['puuid']
    return None


def get_match_ids_paged(puuid: str, max_games: int = 1500) -> List[str]:
    out = []
    start = 0
    while len(out) < max_games:
        count = min(100, max_games - len(out))
        url = f"https://{REGION_HOST}/lol/match/v5/matches/by-puuid/{puuid}/ids?start={start}&count={count}"
        data = riot_get_json(url)
        if not data or not isinstance(data, list):
            break
        out.extend(data)
        if len(data) < count:
            break  # No more matches
        start += count
        time.sleep(1.05)  # polite pacing
    return list(set(out))

def get_match_data(match_id: str) -> Dict:
    url = f"https://{REGION_HOST}/lol/match/v5/matches/{match_id}"
    return riot_get_json(url)

# --- Match DataFrame Construction ---
def flatten_participant(part: dict, match_info: dict, match_meta: dict) -> dict:
    # Copy all participant fields
    row = part.copy()
    # Add match-level fields
    for k in [
        'gameMode', 'gameDuration', 'gameStartTimestamp', 'queueId', 'gameCreation',
        'gameEndTimestamp', 'gameId', 'gameName', 'gameType', 'gameVersion', 'mapId',
        'platformId', 'teams', 'tournamentCode'
    ]:
        row[k] = match_info.get(k)
    # Add meta fields
    row['key'] = 'info'
    # Convert nested fields to JSON strings for R compatibility
    for col in ['challenges', 'perks', 'teams', 'missions']:
        if col in row:
            row[col] = json.dumps(row[col], ensure_ascii=False)
    return row

def fetch_matches_for_summoner(summoner: str, tag: str, max_games: int = 1500) -> pd.DataFrame:
    puuid = get_puuid(summoner, tag)
    if not puuid:
        print(f"Could not get PUUID for {summoner}#{tag}")
        return pd.DataFrame()
    match_ids = get_match_ids_paged(puuid, max_games=max_games)
    print(f"Found {len(match_ids)} matches for {summoner}#{tag}")
    rows = []
    for i, match_id in enumerate(match_ids):
        print(f"Fetching match {i+1}/{len(match_ids)}: {match_id}")
        match = get_match_data(match_id)
        if match and 'info' in match and 'participants' in match['info']:
            info = match['info']
            meta = match.get('metadata', {})
            for p in info['participants']:
                row = flatten_participant(p, info, meta)
                rows.append(row)
        time.sleep(1.1)  # polite pacing
    return pd.DataFrame(rows)

if __name__ == "__main__":
    # All 7 summoners (update with your actual summoner names and tags)
    summoners = [
        ("Jocar", "Jocar"),
        ("Paintrain100", "EUW"),
        ("BigFish84", "EUW"),
        ("locked", "EUW"),
        ("blua", "EUW"),
        ("Jesus", "BuBaz"),
        ("Berrett21", "EUW"),
    ]
    all_dfs = []
    # Sleep controls (like mining.R)
    short_sleep_between_calls = 0.10  # seconds
    long_sleep_every = 60  # after every 60 matches
    long_sleep_seconds = 210  # 3.5 minutes
    match_counter = 0
    for idx, (summoner, tag) in enumerate(summoners):
        df = fetch_matches_for_summoner(summoner, tag, max_games=1500)  # or your desired max
        if not df.empty:
            df['summonerName'] = summoner
            all_dfs.append(df)
            match_counter += len(df)
        # Long sleep after every batch of matches
        if long_sleep_every > 0 and match_counter > 0 and match_counter % long_sleep_every == 0:
            print(f"Long sleep: {long_sleep_seconds}s after {match_counter} matches")
            time.sleep(long_sleep_seconds)
        else:
            time.sleep(short_sleep_between_calls)
    if all_dfs:
        result = pd.concat(all_dfs, ignore_index=True)
        # Reorder columns to match R as much as possible
        preferred_cols = [
            'gameMode', 'gameDuration', 'gameStartTimestamp', 'queueId', 'championName',
            'kills', 'deaths', 'assists', 'lane', 'summonerName', 'summonerLevel', 'win', 'key',
            'gameCreation', 'gameEndTimestamp', 'gameId', 'gameName', 'gameType', 'gameVersion', 'mapId'
        ]
        # Add all other columns after preferred
        cols = preferred_cols + [c for c in result.columns if c not in preferred_cols]
        result = result[cols]
    out_path = os.path.join('data', 'all_games_new.csv')
    result.to_csv(out_path, index=False)
    print(f"Saved to {out_path}")
    else:
        print("No data fetched.")
