
Robust multi-label sum-product networks for binary relevance problems (multilabel-cspn)  
===============================  
  
This code was used to get experiments results for the paper "Skeptical binary inferences in multi-label problems with sets of probabilities". 

This implementation uses some piece of code from the repository github: 
* [https://github.com/AlCorreia/SUM2019](https://github.com/AlCorreia/SUM2019)

SUM2019 implements a scalable and robust sum-product network for classification problems, based on an implementation of LearnSPN [1] in R. 
with a few important contributions:  

1. **Robust estimates** [2],  
2. **Class-selective SPNs**, 
3.  **Memory caches**. 

For further details, we refer to [package](https://github.com/AlCorreia/SUM2019).

Installation / Usage  
--------------------  
  
To clone the repo:  
  
 $ git clone https://github.com/salmuz/multilabel_cspn.git 
  
To usage:
  
The experiments in the paper can be reproduced with the `R/mlc.resampling.r` script, but it is necessary to generate the datasets by using the script: 

https://github.com/salmuz/classifip/blob/master/experiments/classification/mlc/mlc_resampling.py
  
Author  
------  
* Yonatan-Carlos Carranza-Alarcon  
  
Example  
-------  
Very soon 
<!---
There are many tests example in the tests folder.  
* Cross-validation with n-time repetition.  
-->
  
References
------- 
[1] Gens, Robert, and Domingos Pedro.  
"Learning the structure of sum-product networks."  
International conference on machine learning. 2013.  
  
[2] Mauá, Denis Deratani, et al.  
"Robustifying sum-product networks."  
International Journal of Approximate Reasoning 101 (2018): 163-180.  
  
[3] Correia, Alvaro HC, and Cassio P. de Campos. "Towards scalable and robust sum-product networks."   
International Conference on Scalable Uncertainty Management. Springer, Cham, 2019.


-------  
If this material has been useful to you, please consider citing  
  
Yonatan Carlos Carranza Alarcón, Sébastien Destercke. "Skeptical binary inferences in multi-label problems with sets of probabilities"  
  
```  
@misc{https://doi.org/10.48550/arxiv.2205.00662,
  doi = {10.48550/ARXIV.2205.00662},
  url = {https://arxiv.org/abs/2205.00662},
  author = {Alarcón, Yonatan Carlos Carranza and Destercke, Sébastien},
  keywords = {Machine Learning (stat.ML), Artificial Intelligence (cs.AI), Machine Learning (cs.LG), Combinatorics (math.CO), FOS: Computer and information sciences, FOS: Computer and information sciences, FOS: Mathematics, FOS: Mathematics},
  title = {Skeptical binary inferences in multi-label problems with sets of probabilities},
  publisher = {arXiv},
  year = {2022}
} 
```  
