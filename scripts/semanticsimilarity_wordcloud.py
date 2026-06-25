import pandas as pd
from wordcloud import WordCloud
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, Normalize
import numpy as np

# Load data
df = pd.read_csv("/home/leah/Git/InteroMentalHealth/data/semantic_similarity/semantic_similarity_ALL.csv", names=["Scale", "Similarity"])

# Clean up labels (optional)
df['Label'] = df['Scale'].str.replace(r"\(.*?\)", "", regex=True).str.strip()

# Create frequency dict
frequencies = dict(zip(df['Label'], df['Similarity']))

# Normalize similarities
norm = Normalize(vmin=min(frequencies.values()), vmax=max(frequencies.values()))

# Custom red-purple-blue colormap
custom_cmap = LinearSegmentedColormap.from_list(
    "custom_rpb", ["#4a6fe3", "HotPink", "#d33f6a"]
)

# Color function based on value
def color_by_similarity(word, font_size, position, orientation, font_path, random_state):
    value = frequencies.get(word, 0)
    rgba = custom_cmap(norm(value))
    return "#{:02x}{:02x}{:02x}".format(
        int(rgba[0]*255), int(rgba[1]*255), int(rgba[2]*255)
    )

# Build word cloud
wc = WordCloud(width=1200, height=600, background_color="white", random_state=13).generate_from_frequencies(frequencies)

# Recolor with value-based function
wc.recolor(color_func=color_by_similarity)

# Plot word cloud and colorbar
fig, ax = plt.subplots(figsize=(14, 7))
ax.imshow(wc, interpolation="bilinear")
ax.axis("off")

# Create a colorbar separately
from matplotlib.cm import ScalarMappable
sm = ScalarMappable(cmap=custom_cmap, norm=norm)
sm.set_array([])  # needed for colorbar to work

# Add colorbar
cbar = fig.colorbar(sm, ax=ax, orientation="vertical", fraction=0.046, pad=0.04)
cbar.set_label('Semantic Similarity', fontsize=14)
cbar.ax.tick_params(labelsize=12)

plt.tight_layout()
plt.show()

fig.savefig("/home/leah/Git/InteroMentalHealth/figures/Semantic_similarity_maia/semantic_wordcloud_colorbar.pdf", dpi=300)

