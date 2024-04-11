import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pickle
from statistics import median
from pybaseball import statcast_pitcher

data = pd.read_csv("../Data/stats.csv")
tjs = pd.read_csv("../Data/tjs.csv")

data_id = data[["last_name, first_name", "player_id"]]
tjs_id = tjs.merge(
	right = data_id,
	how = "inner",
	on = "last_name, first_name"
	)

tjs_id.drop_duplicates(
	subset = "player_id",
	inplace = True
	)

# tjs_id.to_csv("../Data/tjs_clean.csv", index = False)

pitchers = tjs_id["player_id"].tolist()

no_tjs = data["player_id"].sample(n = 100).tolist()

pitchers_scrape = list(set(pitchers + no_tjs))

# pitchers = {
# 	# tj
# 	# 592662: "Ray, Robbie",
# 	434378: "Verlander, Justin",
# 	# 592789: "Syndergaard, Noah",
# 	# 519242: "Sale, Chris",
# 	456501: "Cueto, Johnny",
# 	628317: "Maeda, Kenta",
# 	645261: "Alcantara, Sandy",
# 	# 663554: "Mize, Casey",
# 	# 607644: "Means, John",
# 	502154: "Britton, Zack",
# 	622780: "Perdomo, Angel",
# 	663556: "McClanahan, Shane",
# 	571510: "Boyd, Matthew",
# 	650671: "Quijada, Jose",
# 	605488: "Springs, Jeffrey",
# 	572020: "Paxton, James",
# 	# no tj
# 	605483: "Snell, Blake",
# 	425844: "Greinke, Zack",
# 	543037: "Cole, Gerrit",
# 	453286: "Scherzer, Max",
# 	573186: "Stroman, Marcus",
# 	477132: "Kershaw, Clayton",
# 	# 433587: "Hernandez, Felix"
# 	}

seasons = list(range(2015, 2023 + 1))

# # https://baseballsavant.mlb.com/leaderboard/custom?year=2023%2C2022%2C2021%2C2020%2C2019&type=pitcher&filter=&min=q&selections=player_age%2Cp_game%2Cp_formatted_ip%2Cpitch_count%2Cpitch_hand&chart=false&x=player_age&y=player_age&r=no&chartType=beeswarm&sort=player_name&sortDir=desc
# player_pitch_count = pd.read_csv("../Data/player_pitch_count.csv")

# pitch_n_threshold = int(median(player_pitch_count["pitch_count"]))

# plt.hist(player_pitch_count["pitch_count"])
# plt.axvline(x = pitch_n_threshold, color = "C1", linestyle = "--", label = "Median")
# plt.xlabel("Season Pitch Count")
# plt.ylabel("Pitcher Count")
# plt.legend()
# plt.savefig("../Images/pitch_count_hist.png")
# plt.show()

# player_pitch_count = player_pitch_count[player_pitch_count["pitch_count"] >= pitch_n_threshold]
# player_id = player_pitch_count["player_id"].tolist()

# print("Pitcher Count:", len(player_id))
# print("Pitch Count:", player_pitch_count["pitch_count"].sum())

opening_days = ["2015-04-05", "2016-04-03", "2017-04-02", "2018-03-29",
	"2019-03-20", "2020-07-23", "2021-04-01", "2022-04-07",
	"2023-03-30"]
final_days = ["2015-10-05", "2016-10-02", "2017-10-01", "2018-10-01",
	"2019-09-29", "2020-09-27", "2021-10-03", "2022-10-05",
	"2023-10-01"]
# opening_day = "2019-03-20"
# final_day = "2019-09-29"

# print(tj.values())
# for i in tj:
# 	print(tj[i])

for i in pitchers_scrape:
	for j in range(0, len(seasons)):
		file_ij = "player_" + str(i) + "_" + str(seasons[j]) + ".csv"
		print(file_ij)

		data_ij = statcast_pitcher(
			opening_days[j],
			final_days[j],
			i
			)
		data_ij.to_csv("../Data/" + file_ij, index = False)

# stats = {i: {} for i in pitchers}
# stats_file = "../Data/player_pitches_2015_2023.pkl"


# # for i in tj

# for i in pitchers:
# 	for j in range(0, len(seasons)):
# 		print(i)
# 		print(seasons[j])

# 		stats[i][seasons[j]] = statcast_pitcher(
# 			opening_days[j],
# 			final_days[j],
# 			i
# 			)

# # print(stats)

# with open(stats_file, "wb") as f:
# 	pickle.dump(stats, f)
