'''
Exploring K-means for finding groups of states.

Scott Spurlock 10/25/17
'''
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import numpy as np

state_names = ['AL',
            'AK',
            'AZ',
            'AR',
            'CA',
            'CO',
            'CT',
            'DE',
            'FL',
            'GA',
            'HI',
            'ID',
            'IL',
            'IN',
            'IA',
            'KS',
            'KY',
            'LA',
            'ME',
            'MD',
            'MA',
            'MI',
            'MN',
            'MS',
            'MO',
            'MT',
            'NE',
            'NV',
            'NH',
            'NJ',
            'NM',
            'NY',
            'NC',
            'ND',
            'OH',
            'OK',
            'OR',
            'PA',
            'RI',
            'SC',
            'SD',
            'TN',
            'TX',
            'UT',
            'VT',
            'VA',
            'WA',
            'WV',
            'WI',
            'WY']

# Average winter temp (F), avergage snowfall (inches)
state_weather = [
        [46.5, 1.6],
        [2.6, 74.5],
        [43.6, 0.3],
        [41.5, 5.2],
        [46.2, 0],
        [25.8, 19.1],
        [28.5, 40.5],
        [36.1, 20.2],
        [59.4, 0],
        [47.8, 0.7],
        [67.4, 0],
        [25.4, 19.2],
        [28.3, 24.6],
        [29.4, 25.9],
        [21.7, 34.9],
        [31.9, 14.7],
        [35.9, 12.5],
        [50.9, 0],
        [16.8, 61.8],
        [34.7, 20.2],
        [27.4, 43.8],
        [21.7, 51.1],
        [12.4, 54],
        [46.7, 0.9],
        [32.3, 17],
        [21.2, 38.1],
        [25.7, 25.9],
        [32.2, 21.8],
        [21.1, 60.8],
        [33, 16.5],
        [36.1, 9.6],
        [23.3, 123.8],
        [42.1, 7.6],
        [12.2, 51.2],
        [29.5, 27.5],
        [39.1, 7.8],
        [34, 3],
        [28.4, 28.2],
        [31.4, 33.8],
        [46.1, 0.5],
        [19.5, 43.9],
        [39.1, 6.3],
        [47.9, 1.5],
        [28.2, 56.2],
        [19.4, 81.2],
        [36.8, 10.3],
        [33, 5],
        [32.8, 62],
        [17.2, 50.9],
        [21.2, 91.4]]

# ---------------------------------------------------------------------
# Turn Python list into Numpy array
data = np.array(state_weather)
num_states = data.shape[0]

# How many clusters to find
k = 4

print('State weather data (Average winter temp (F),',
      'avergage snowfall (inches)):')
print(data)

# ---------------------------------------------------------------------
print('Running Kmeans on list of', num_states, 'states...')
clt = KMeans(n_clusters=k)
clt.fit(data)
print('Kmeans complete')
# Assign each location to the closest cluster center
labels = clt.predict(data)

# ---------------------------------------------------------------------
# Visualization
colors =  ['b', 'r', 'g', 'c', 'm', 'y', 'k']
plt.figure()
# Plot states one at a time
for i in range(num_states):
    curr_label = labels[i]
    curr_color = colors[curr_label]
    plt.scatter(data[i, 0], data[i, 1], c=curr_color, s=60)
    plt.text(data[i, 0]+0.5, data[i, 1], state_names[i], size=10)

plt.title('State Weather Data, K = ' + str(k))
plt.xlabel('Average Winter Temp. (F)')
plt.ylabel('Average Snowfall (in)')
plt.show()
