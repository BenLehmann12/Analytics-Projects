This project builds a machine learning pipeline to replace expensive simulations with fast predictive models and enable scalable optimization.

The workflow combines:
- Nonlinear dimensionality reduction
- Deep learning-based surrogate modeling
- Generative modeling for synthetic data
- Optimization in latent space

Goal: Create accurate ML pipeline and enable efficient optimization.

1. Dimensionality Reduction
Applied Principal Geodesic Analysis (PGA) to capture nonlinear structure in data
Compared against PCA and demonstrated improved representation of complex relationships

3. Surrogate Modeling
Built a PyTorch neural network to predict target variables (𝐶𝑙,𝐶𝑑,𝐶𝑚)
Achieved ~95% accuracy on validation data

5. Generative Modeling
Trained a Variational Autoencoder (VAE) on 19k+ samples
Learned a continuous latent space for generating realistic new designs

7. Feature Importance
Analyzed relationships between input features and performance metrics
Identified key drivers influencing optimization objectives

9. Optimization
Performed latent space search to maximize objective function
Generated candidate solutions under varying constraints


Tools Used: Python, PyTorch, NumPy, Pandas, Scikit-learn, Matplotlib
