'''
Exploring K-means for finding the dominant colors in an image and performing
color quantization.

Adapted from Adrian Rosebrock
'''

# import the necessary packages
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import cv2

def centroid_histogram(clt):
    # grab the number of different clusters and create a histogram
    # based on the number of pixels assigned to each cluster
    numLabels = np.arange(0, len(np.unique(clt.labels_)) + 1)
    (hist, _) = np.histogram(clt.labels_, bins = numLabels)

    # normalize the histogram, such that it sums to one
    hist = hist.astype("float")
    hist /= hist.sum()

    # return the histogram
    return hist

def plot_colors(hist, centroids):
    # initialize the bar chart representing the relative frequency
    # of each of the colors
    bar = np.zeros((50, 300, 3), dtype = "uint8")
    startX = 0

    # loop over the percentage of each cluster and the color of
    # each cluster
    for (percent, color) in zip(hist, centroids):
        # plot the relative percentage of each cluster
        endX = startX + (percent * 300)
        cv2.rectangle(bar, (int(startX), 0), (int(endX), 50),
            color.astype("uint8").tolist(), -1)
        startX = endX

    # return the bar chart
    return bar

# --------------------------------------------------------------------
# Set values
image_path = 'RainbowBlocks.jpg'
k = 250

print('Starting...')

# load the image
image = cv2.imread(image_path)

if image is None:
    print('********************************************')
    print('*** Unable to load image', image_path)
    print('********************************************\n')

# Resize to 25% to make things faster
image = cv2.resize(image, None, image, fx=0.25, fy=0.25)

# Convert it from BGR to RGB so that we can dispaly it with matplotlib
image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
h, w, ch = image.shape

print('Image loaded (', h, 'x', w, ')')

# show our image
plt.figure()
plt.axis("off")
plt.imshow(image)

# reshape the image to be a list of pixels
pixels = image.reshape((h * w, 3))
pixels = pixels.astype('float')

# cluster the pixel intensities
print('Running Kmeans on list of', pixels.shape[0], 'pixels...')
clt = KMeans(n_clusters=k)
clt.fit(pixels)
print('Kmeans complete')

# Assign each pixel to the closest cluster center
labels = clt.predict(pixels)

# Get the list of cluster centers
colors = clt.cluster_centers_.astype('uint8')

# Assign each pixel to be the color of its closest cluster center
quant = colors[labels]

# reshape the list of pixels to be an image again
quant = quant.reshape((h, w, 3))

# build a histogram of clusters and then create a figure
# representing the number of pixels labeled to each color
hist = centroid_histogram(clt)
bar = plot_colors(hist, clt.cluster_centers_)

# ---------------------------------------------------------------------
# Show 3D plot of all the pixels in terms of red, green, and blue
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.set_title('RGB')
ax.axis('equal')
ax.set_xlabel('R')
ax.set_ylabel('G')
ax.set_zlabel('B')
ax.set_xlim([0, 1])
ax.set_ylim([0, 1])
ax.set_zlim([0, 1])
data = pixels.astype('float')/255.0
subsample = 5
data = data[::subsample]
ax.scatter(data[:, 0], data[:, 1], data[:, 2], c=data)
plt.show()

# show our color bar
plt.figure()
plt.axis("off")
plt.imshow(bar)
plt.show()

# show quantized image
plt.figure()
plt.axis("off")
plt.imshow(quant)
plt.show()
