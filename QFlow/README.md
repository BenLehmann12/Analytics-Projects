Overview

Built an end-to-end machine learning and deep learning pipeline to classify simulated quantum device states and analyze quantum circuit topology using computer vision, graph neural networks, and classical machine learning.

This project combines:

 - Graph Neural Networks (GNNs) for circuit topology and placement prediction
 - Convolutional Neural Networks (CNNs) for 2D sensor map classification
 - Classical ML models for benchmarking and feature-based classification
 - Dimensionality reduction & visualization for feature space analysis
 - Synthetic data generation for dataset augmentation
   
**Project Goals:**
 - Classify quantum device states from simulated sensor data:
     - Barrier
     - Single Dot
     - Double Dot
     - Short Circuit
- Predict node-level and graph-level states using graph representations of circuits
- Benchmark deep learning models against classical machine learning baselines
- Analyze robustness under noisy sensor conditions
- Visualize and interpret high-dimensional feature spaces


**Technical Highlights**
Graph Neural Networks (PyTorch Geometric)
Implemented and compared:
 - Graph Convolutional Networks (GCN)
 - Graph Attention Networks (GAT)
 - GraphSAGE
   
Applications:
 - Node-level state prediction
 - Graph-level device classification
 - Attention weight visualization for graph interpretability

Key graph design:
 - Converted 100×100 device maps into graph structures
 - Represented circuits as Directed Acyclic Graphs (DAGs)
 - Engineered node features from:
      - Current maps
      - Charge sensor maps
      - Occupancy information


**Computer Vision / CNN Models**
Built CNN architectures for:

Device state recognition from 100×100 current maps
Dot count detection
Empty vs filled region classification

Experiments included:
 - Architecture comparison
 - Pooling strategy tuning
 - Dense layer optimization
 - Performance benchmarking
 - Classical Machine Learning

Benchmarked against:
 - Support Vector Machines (SVM)
 - Random Forest
 - Gradient Boosting / XGBoost
 - Baseline neural networks

Feature engineering:
 - Flattened current/sensor maps
 - PCA-transformed features
 - Statistical summary features
 - Feature Engineering & Dimensionality Reduction

Applied:
 - PCA
 - t-SNE
 - UMAP

Use cases:
 - Feature importance analysis
 - State clustering visualization
 - High-dimensional similarity analysis

Generated:
 - Cosine similarity matrices
 - Euclidean distance matrices
 - Mahalanobis distance analysis
 - Noise Robustness Analysis

Tested model stability under:
 - Gaussian noise
 - Poisson noise

Measured:
 - Classification degradation
 - Robustness across architectures


**Synthetic Data Generation**
Explored:
 - Generative Adversarial Networks (GANs)

Goals:
 - Generate synthetic quantum dot maps
 - Improve dataset diversity
 - Support future augmentation workflows
 - Dataset

Source: QFlow Lite simulated quantum device dataset

Input modalities:
 - Current maps
 - Charge sensor maps
 - Occupancy maps

Input shape: 100 × 100 spatial maps

Challenge:
 - Significant class imbalance:
 - Single Dot heavily dominant
 - Barrier minority class
 - Short Circuit absent in QFlow Lite subset  (Will work on the New Dataset)

Handled through:
 - Label remapping
 - Class-aware evaluation
 - Robust preprocessing pipelines
 - Key Results
 - Built full preprocessing, training, and evaluation pipeline in Python

Successfully trained:
 - CNN classifiers
 - GNN architectures
 - Classical ML baselines

Key findings:
 - CNNs outperform classical models on raw image-like sensor maps
 - Classical models remain competitive with engineered features
 - GNNs effectively capture spatial and topological relationships
 - Feature space clustering clearly separates Single Dot and Double Dot states

Skills Demonstrated:
 - Python
 - PyTorch
 - PyTorch Geometric
 - Scikit-learn
 - NumPy / SciPy
 - Matplotlib
 - Deep Learning
 - Graph Machine Learning
 - Computer Vision
 - Feature Engineering
 - Dimensionality Reduction
 - Model Evaluation
 - Data Preprocessing
