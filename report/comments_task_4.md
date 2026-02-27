# Decision Tree, Bagging, and Random Forest  
Dataset: Breast Cancer Wisconsin (Diagnostic)

---

## (a) Single Decision Tree

The dataset was split into 70% training and 30% testing data.  
A classification tree was trained using the `rpart()` function.

### Results with Different Random Seeds

| Seed | Accuracy | AUC |
|------|----------|------|
| 1 | 0.8889 | 0.8871 |
| 2 | 0.9415 | 0.9455 |
| 3 | 0.9532 | 0.9588 |

### Comments

- Accuracy ranges from **0.89 to 0.95**.
- AUC ranges from **0.887 to 0.959**.
- The variation across different seeds is noticeable (~6%).

This indicates that a single Decision Tree has **high variance**.  
Its performance is sensitive to how the data is split into training and testing sets.

---

## (b) Bagging (B = 200)

Bootstrap sampling was applied 200 times on the training set.  
A classification tree was trained on each bootstrap sample, and predictions were aggregated by averaging predicted probabilities (threshold = 0.5).

### Results

- **Accuracy:** 0.9415  
- **AUC:** 0.9904  

### Comments

- Accuracy is higher and more stable compared to a single tree.
- AUC increases significantly (~0.99), indicating excellent class separation.
- Bagging reduces variance by averaging multiple models.

---

## (c) Variance Comparison (50 Repetitions)

The entire train/test procedure was repeated 50 times.

### Single Tree

- Mean Accuracy: **0.9270**
- Variance: **0.0003409**

### Bagging

- Mean Accuracy: **0.9416**
- Variance: **0.0002785**

### Variance Reduction

Variance Reduction = **18.32%**

### Comments

- Bagging improves the average accuracy.
- More importantly, the variance decreases by approximately **18%**.
- This confirms the theoretical expectation:  
  > Bagging reduces the variance of high-variance models such as Decision Trees.

---

## (d) Random Forest (ntree = 200)

A Random Forest model was trained and compared with Bagging.

### Results

| Model | Accuracy | AUC |
|--------|----------|------|
| Bagging | 0.9357 | 0.9807 |
| Random Forest | 0.9474 | 0.9832 |

### Comments

Random Forest achieves better performance than Bagging in both Accuracy and AUC.

### Explanation of the Difference

**Bagging:**
- Uses bootstrap sampling
- Each tree uses all available features

**Random Forest:**
- Uses bootstrap sampling
- Performs random feature selection at each split

Random feature selection:
- Reduces correlation between trees
- Increases model diversity
- Further decreases variance

As a result, Random Forest has higher performance than standard Bagging.

---

## Conclusion

- A single Decision Tree exhibits high variance and instability.
- Bagging improves stability and reduces variance (~18% reduction).
- Random Forest further enhances performance through random feature selection.
- Ensemble methods are particularly effective when the base learner has high variance, such as Decision Trees.