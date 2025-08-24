import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os

# Load data
df = pd.read_csv('data/all_games_new.csv')

os.makedirs('reports', exist_ok=True)

# Games per Summoner
plt.figure(figsize=(8, 5))
games = df['summonerName'].value_counts().sort_values()
sns.barplot(x=games.values, y=games.index, palette='viridis')
plt.title('Games per Summoner')
plt.xlabel('Number of Games')
plt.ylabel('Summoner')
plt.tight_layout()
plt.savefig('reports/games_per_summoner.png')
plt.close()

# Win Rate by Summoner
winrate = df.groupby('summonerName').agg(games=('win', 'count'), wins=('win', 'sum'))
winrate['winrate'] = winrate['wins'] / winrate['games']
winrate = winrate.sort_values('winrate')
plt.figure(figsize=(8, 5))
sns.barplot(x=winrate['winrate'], y=winrate.index, palette='mako')
plt.title('Win Rate by Summoner')
plt.xlabel('Win Rate')
plt.ylabel('Summoner')
plt.xlim(0, 1)
plt.gca().xaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: f'{x:.0%}'))
plt.tight_layout()
plt.savefig('reports/winrate_by_summoner.png')
plt.close()

# Top 10 Champions Played
champ_counts = df['championName'].value_counts().head(10).sort_values()
plt.figure(figsize=(8, 5))
sns.barplot(x=champ_counts.values, y=champ_counts.index, palette='crest')
plt.title('Top 10 Champions Played')
plt.xlabel('Games Played')
plt.ylabel('Champion')
plt.tight_layout()
plt.savefig('reports/top_champions.png')
plt.close()

# KDA Distribution
plt.figure(figsize=(8, 5))
df['kda'] = (df['kills'] + df['assists']) / df['deaths'].replace(0, 1)
sns.histplot(df['kda'], bins=30, color='#69b3a2', kde=True)
plt.title('KDA Distribution')
plt.xlabel('KDA')
plt.ylabel('Count')
plt.tight_layout()
plt.savefig('reports/kda_distribution.png')
plt.close()
