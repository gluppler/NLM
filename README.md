# NLM
## Project Description: No Language Matters

**No Language Matters** is a dynamic text processing application designed to analyze and mutate linguistic input in various ways. The project aims to explore the nuances of language by aggregating fuzzy word frequencies, generating mutations of textual data, and calculating average character frequencies. This application leverages principles of functional programming and fuzzy logic to create a unique environment for examining language patterns, emphasizing that communication transcends linguistic barriers.

### Key Features:

- **Text Preprocessing**: The application preprocesses input text by converting it to lowercase and removing non-alphabetic characters, enabling accurate analysis.
- **Tokenization**: The input text is tokenized into individual words for further processing and analysis.
- **Fuzzy Matching**: It implements a fuzzy matching algorithm based on the Levenshtein distance to group similar words, facilitating the aggregation of word frequencies.
- **Mutation Generation**: The application generates multiple variations of input tokens through random mutations, allowing for exploration of language transformation.
- **Character Frequency Analysis**: It calculates the frequency of characters in each generation of tokens, providing insights into language usage patterns.
- **CSV Export**: The results, including generations, fuzzy word frequencies, and average character frequencies, can be exported to a CSV file for further analysis or reporting.

## Fuzzy Logic Diagram

### Step 1: **Input Text Collection**
- **Description**: The user provides a text input that is to be analyzed by the system.
- **Goal**: Obtain raw text data.

---

### Step 2: **Text Preprocessing**
- **Description**: The input text undergoes preprocessing:
  - Convert to lowercase (for case-insensitive comparison).
  - Remove stop words (e.g., "the", "is", "and").
  - Tokenize the text (split it into words).
- **Goal**: Clean the text for consistent analysis.

---

### Step 3: **Token Mutation (Multiple Generations)**
- **Description**: The tokens are mutated across generations by randomly altering characters in words to generate variations of the text.
- **Goal**: Simulate language evolution or changes over multiple generations.

---

### Step 4: **Fuzzy Matching (Levenshtein Distance)**
- **Description**: Compare the words within each generation using the **Levenshtein distance** algorithm. This measures how many edits (insertion, deletion, or substitution) are required to transform one word into another.
  - Words within a certain threshold (e.g., distance ≤ 2) are grouped as "similar".
- **Goal**: Identify words that are close in spelling, even if they are not identical.

---

### Step 5: **Fuzzy Frequency Aggregation**
- **Description**: For each generation, count how many times each fuzzy group of words appears. This creates fuzzy word frequency statistics.
- **Goal**: Measure the relative importance or occurrence of similar words across generations.

---

### Step 6: **Character Frequency Calculation**
- **Description**: For each generation, the frequency of individual characters is also calculated, providing insights into which letters are most common across the mutations.
- **Goal**: Analyze character-level data alongside word-level fuzzy frequencies.

---

### Step 7: **CSV Export**
- **Description**: The system exports the fuzzy frequencies and character averages to a CSV file, making the data accessible for further analysis or visualization.
- **Goal**: Save results for reporting and further research.

---

### Step 8: **Optional Machine Learning Predictions**
- **Description**: The aggregated fuzzy word frequencies can be passed to a machine learning model (trained separately) for pattern recognition or predictions based on the language evolution data.
- **Goal**: Incorporate predictive analysis using the generated fuzzy data.

---

### Step 9: **User Output**
- **Description**: The system outputs both the fuzzy word frequencies and character frequency data to the user.
- **Goal**: Display the results of the fuzzy logic analysis.

---

This breakdown illustrates how fuzzy logic is applied step-by-step in the **No Language Matters** project, focusing on text comparison, mutation, and aggregation of similar data.

## How-To Guide

### Prerequisites

- Haskell installed on your machine.
- Required libraries for file handling and random number generation.

### Setup

1. **Clone the Repository**:
   ```bash
   git clone <repository-url>
   cd nlm
   ```

2. **Install Dependencies**:
   Ensure you have all necessary libraries installed. You may need to run:
   ```bash
   cabal update
   cabal install
   ```

### Running the Application

1. **Compile the Project**:
   In your terminal, navigate to the project directory and run:
   ```bash
   cabal build
   ```

2. **Execute the Main Module**:
   Run the main module to start the application:
   ```bash
   cabal run
   ```

3. **Input Text**:
   When prompted, enter the text you want to analyze.

4. **Specify Generations**:
   Enter the number of generations (mutations) you want to simulate.

5. **Export Results**:
   When prompted, provide a filename (e.g., `output.csv`) to save the results. The application will generate a CSV file containing the following:
   - Generations of mutated tokens
   - Fuzzy word frequencies for each generation
   - Average character frequencies across generations

### Analyzing Results

- Open the generated CSV file using a spreadsheet application (e.g., Microsoft Excel, Google Sheets) to visualize the results.
- Explore the fuzzy frequencies, character frequencies, and the relationships between them to gain insights into language patterns.

### Conclusion

**No Language Matters** serves as a robust tool for linguists, researchers, and enthusiasts to delve into the intricacies of language processing. By integrating various functional programming principles and fuzzy logic, it highlights the importance of understanding language beyond its surface.

--- 

