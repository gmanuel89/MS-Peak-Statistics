# MS PEAK STATISTICS

***

## Program version
In order for this WIKI to be applicable, the version of the program must be equal to or higher than **2017.05.31.0**.

***

## Programming language
[R: The Comprehensive R Archive Network](https://www.r-project.org/)

***

## Scope of the software
The software imports the peaklist matrix exported from other pieces of software (e.g. [MS PEAKLIST EXPORT](https://github.com/gmanuel89/MS-Peaklist-Export), [SCiLS Lab](http://scils.de/), [PEAKS Studio](http://www.bioinfor.com/peaks-studio/) or [Progenesis QI for Proteomics](http://www.nonlinear.com/progenesis/qi-for-proteomics/)), in which each row is a patient's spectrum (single average mass spectrum or individual spectra/pixels) and each column is a peak in the mass spectrometric dataset, along with other demographical information (such as age, sex or class), and performs different statistical analyses (correlation analysis, two-level effect statistical analysis and multi-level effect statistical analysis). All the statistical analyses can be perform either with outliers included or excluded (employing the interquartile range rule).

The software first separates mass spectrometric data from demographical data (according to what the user specifies during the import phase), allows the user to select a discriminatory attribute (to be used in statistical analysis as class/outcome/response variable) and demographical attributes for the different statistical analysis.

After identifying all the data types, the software allows data transformation, in order to change the distribution type of the variables (e.g. from non-normal to normal). Then, the software computes correlation analysis between each spectral feature (peak) and selected demographical data and prints correlation scatter plots.

Moreover, the software computes two-level and multi-level effect statistical analysis, by seeing the discriminant attribute respectively as a two-level factor variable (0, 1) and a multi-level factor variable (1, 2, 3, 4, ...).

The software operates as follows, for each mass spectrometric feature (peak):
    * The distribution of the signal intensity is evaluated by the Shapiro-Wilk test (both in two-level and multi-level effect analysis), in order to define if the data is normally or non-normally distributed.
    * The equality of variances in the two-level effect analysis is evaluated by the variance F test (for normal data) and the Levene test (for non-normal data).
    * The differently expressed signals in the two-level effect analysis are evaluated by the statistical tests with a set level of alpha (p-value threshold): t-test (for normal data with equal and unequal variances), Wilcoxon rank-sum test (for non-normal data with equal variances), Kolmogorov-Smirnov Test (for non-normal data with unequal variances).
    * The equality of variances in the multi-level effect analysis is evaluated by the Bartlett test (for normal data) and the Levene test (for non-normal data).
    * The differently expressed signals in the multi-level effect analysis are evaluated by the statistical tests with a set level of alpha (p-value threshold): ANOVA (for normal data with equal variances), Kruskal-Wallis rank-sum test (for non-normal data with equal variances), Welch test (for normal data with unequal variances) and Permutation test (for non-normal data with unequal variances).
    * Pot-hoc tests are then performed in order to correct the p-value for multiple comparisons in the multi-level effect analysis.

The software can be also applicable to any kind of data, since all it takes is a matrix in which each row is an observation and each column is a feature, and among the features some are demographical data.

***

## Type of data and its organization

#### Type of data
The software imports a CSV matrix file.

#### Output data
The software generates a CSV file corresponding to the peaklist intensity matrix, in which each row is a spectrum (pixel or patient) and each column is a peak,with additional "Sample" and "Class" columns.

The spectra files are placed in a folder with the same name as the peaklist file: if "MSD" is selected as the file format, one MSD file for each spectrum is generated, with the peaks embedded in the same file; if "TXT" is selected as the file format, two TXT files are generated, one forthe spectrum and one for the peaks.

***

## Buttons, entries and operations

* **Correlation analysis**: defines if the correlation analysis between each feature and the response variable (discriminant attribute) should be computed, by estimating the Pearson's or Spearman's rho.

* **Remove outliers Correlation analysis**: defines if the outlier removal should be performed when computing the correlation analysis.

* **Plot correlation graphs**: defines if the correlation graphs should be saved as images.

* **Two-level effect analysis**: defines if the two-level effect analysis between each feature and the response variable (discriminant attribute) should be computed.

* **Remove outliers Two-level effect analysis**: defines if the outlier removal should be performed when computing the two-level effect analysis.

* **Cumulative class in the Two-level effect analysis**: defines if the discriminant attribute (response variable) should be seen as a cumulative variable (if numeric): at each iteration, one value of the variable is set as 0 and the values higher than that value are all set as 1.

* **Multi-level effect analysis**: defines if the multi-level effect analysis between each feature and the response variable (discriminant attribute) should be computed.

* **Remove outliers Multi-level effect analysis**: defines if the outlier removal should be performed when computing the multi-level effect analysis.

* **Data transformation**: defines if the imported data (excluding the demographical data) should undergo transformation ("Square root", "Natural logarithm", "Decimal logarithm", "Binary logarithm", "Absolute value", "Sine", "Cosine", "Exponential").

* **Data REC**: defines if a copy of the original data should be saved in the output folder.

* **Output file type**: sets the type of the exported table files ("CSV", "XLS" or "XLSX").

* **Image file type**: sets the type of the exported image files ("PNG", "JPG" or "TIFF").

* **p-value for signal expression difference**: sets the alpha value (p-value threshold) for detecting differentially expressed signals when employing statistical tests.

* **p-value for significance in statistical tests**: sets the alpha value (p-value threshold) for significance when employing statistical tests for variance equality estimation and normal distribution assessment.

* **Minimum number of patients for tests**: selects the minimum number of patients that should be present when running statistical tests.

* **Allow parallel processing**: enables the parallel computation (multi-CPU computation).

* **Browse output folder...**: selects the folder in which all the output files should be saved.

* **Import file...**: imports the matrix file with all the information.

* **Run statistics**: runs the software.

* **Quit**: close the program and the R session.
