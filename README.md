# Development of a movie recommendation engine
## File Structure
1.  Development of a movie recommendation engine.pdf (Working Paper)
2. main.R (ML Algorithms: Logistic Regression, k-NN, Plot Based Recommender, Collaborative Filtering, Decision Trees)
3. Data Processing.R (Data Preparation, Descriptive Statitsics and Visualisations)
4. RShiny Algorithm Comparison.R (Proof of Concept for comparing the algortihms)

## Abstract
Today, platforms such as Netflix or Amazon offer tens of thousands of films which can be streamed online. In view of this huge amount, the user faces an information overload problem which makes the choice of a movie best suited to his interests and needs time consuming and complicated. In order to increase convenience and quality of movie selection, we present three approaches of a moving recommendation system using machine learning algorithms. The implemented demographic filtering technique allows to suggest movies for unknown users, while the two content-based techniques we implement make use of the past consumer behavior. With a  collaborative-filtering method, we exploit comparisons to other consumers and finally,..]

## Getting Started
### Clone the repository
- In R Studio create a new project and go to repository File > New Project > Version Control > Git.
- Clone the repository https://github.com/git-yifanzhu/machine_learning_hsg20s.git

### Data loading
- Go to data folder and unzip the files from RawData.zip directly into the data folder
- Execute the Data Processing.R file to process (movies.RData) for the main analysis

### Calculate and Build ML algorithms
- Execute the main.R file for estimating the different ML algorithms

### Check the different Movie Suggestions in Shiny App
- Open the Rshiny Algorithm Comparison.R and run the App on your local host
- Please Note: Due to the fact that all algortihms are loaded in the app, it takes about 5 minutes until the app is running since the main.R file will be sourced. Therefore, the app is also deployed under www....)

#add gif to (will be added)
