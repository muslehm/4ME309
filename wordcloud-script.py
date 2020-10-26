from wordcloud import WordCloud, STOPWORDS
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

data = pd.read_csv(r'filtered_blm1002to0916.csv', error_bad_lines=False)

comment_words = ""
stop_words = ["https", "co", "RT", "blacklivesmatter", "amp", "blm", "fuck", "fucking", "shit"] + list(STOPWORDS)

# Iterating through the .csv data file 
for i in data.text: 
    i = str(i) 
    separate = i.split() 
    for j in range(len(separate)): 
        separate[j] = separate[j].lower() 
      
    comment_words += " ".join(separate)+" "

# Creating the Word Cloud
mask = np.array(Image.open('comment.png'))

final_wordcloud = WordCloud(width = 800, height = 800, 
                stopwords = stop_words, 
                mask = mask,
                background_color='navy', 
                colormap='rainbow_r',
                min_font_size = 12).generate(comment_words)

# Displaying the WordCloud                    
plt.figure(figsize = (10, 10), facecolor = None) 
plt.imshow(final_wordcloud) 
plt.axis("off") 
plt.tight_layout(pad = 0) 
  
figure = plt.gcf()
plt.draw()
figure.savefig('wordcloud.png', dpi=100)
plt.show()