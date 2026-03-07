import matplotlib.pyplot as plt
import numpy as np

# ==========================================
# 1. Data Preparation
# ==========================================
# Input observations
x_observations = np.array([3, 2, 1, -1, 0])
y_observations = np.array([4, 2, 3, 1, 0])

# Regression coefficients and means
intercept = 1.2
slope = 0.8
x_mean = 1.0
y_mean = 2.0

# Generate line data
x_range = np.linspace(-3, 4, 100)
y_fitted = intercept + slope * x_range

# ==========================================
# 2. Plotting Configuration
# ==========================================
plt.figure(figsize=(10, 8))
plt.style.use('seaborn-v0_8-muted')

fig, ax = plt.subplots(figsize=(10, 8))

# Move the left and bottom spines to (0,0)
ax.spines['left'].set_position('zero')
ax.spines['bottom'].set_position('zero')

# Hide the top and right spines
ax.spines['right'].set_color('none')
ax.spines['top'].set_color('none')

# (e) Plotting the data points and the regression line
ax.scatter(x_observations, y_observations, color='royalblue', s=100, 
           zorder=3, label='Observations (x, y)')
ax.plot(x_range, y_fitted, color='crimson', linestyle='-', linewidth=2, 
        zorder=2, label=f'Fitted Line: y = {intercept:.1f} + {slope:.1f}x')

# (f) Highlighting the point of means (x_bar, y_bar)
ax.scatter(x_mean, y_mean, color='forestgreen', marker='D', s=150, 
           edgecolor='black', zorder=5, label=f'Mean Point: ({x_mean}, {y_mean})')

# ==========================================
# 3. Labeling and Formatting
# ==========================================
ax.set_title('Simple Linear Regression: Visualizing the Fitted Line and Means', fontsize=14, pad=20)

# Adjust label positions since spines are moved
ax.set_xlabel('x', fontsize=12, loc='right')
ax.set_ylabel('y', fontsize=12, loc='top', rotation=0)

# Set axis limits to ensure (0,0) is centered and all points are visible
ax.set_xlim(-4, 4)
ax.set_ylim(-2, 5)

ax.legend(loc='upper left', frameon=True)
ax.grid(True, linestyle=':', alpha=0.5)

plt.tight_layout()
plt.show()
