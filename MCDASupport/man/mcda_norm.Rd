\name{mcda_norm}
\alias{mcda_norm}
\title{Normalize values using using specified method of the normalization}
\description{
    Performs normalization of the vector using one of supported normalization methods. At present time supported are:

    \tabular{llc}{
        \bold{constant} \tab \bold{method} \tab \bold{B/C} \cr
        LaiHwang \tab Lai-Hwang \tab Y \cr
        linear aggregation \tab linear aggregation \tab Y \cr
        logarithm \tab logarithmic normalization \tab Y \cr
        Markovic \tab Markovic normalization \tab N \cr
        minmax \tab min-max normalization \tab Y \cr
        nonlinear \tab nonlinear normalization \tab Y \cr
        toaverage \tab normalizing to average value \tab N \cr
        tobest \tab normaliring to best value \tab N \cr
        TzengHuang \tab Tzeng-Huang normalization \tab N \cr
        vector \tab vertor normalization \tab Y \cr
        ZavadskasTurskis \tab Zavadskas-Turskis normalization \tab Y \cr
        zscore \tab Z-score \tab N
    }

    B/C means benefit/cost, signifying on if the method requires the user to specify the minmax parameter for normalize function. B/C = N means that the normalize method will not take into account what you specify in minmax parameter.

    \bold{Lai-Hwang}

    Normalize values using approach by Lai and Hwang (1994), see equations bellow.

    For benefit criteria

    \mjsdeqn{z = \frac{x}{max(x) - min(x)}}

    For cost criteria

    \mjsdeqn{z = \frac{x}{min(x) - max(x)}}

    \bold{Linear normalization}

    Normalized values are computed as follows.

    For maximization transformation

    \mjsdeqn{z = \frac{x}{\sum_{i=1}^m x_i}}

    For minimization transformation

    \mjsdeqn{z = \frac{\frac{1}{x}}{\sum_{i=1}^m \frac{1}{x_i}}}

    where m is number of observation in vector to normalize

    \bold{Logarithmic}

    ormalized values are computed as follows.

    For maximization transformation

    \mjsdeqn{z = \frac{ln(x)}{ln(\prod_{i=1}^m x_i)}}

    For minimization transformation

    \mjsdeqn{z = \frac{1 - \frac{ln(x)}{ln(\prod_{i=1}^m x_i)}}{m - 1}}

    where m is number of observation in vector to normalize

    \bold{Markovic normalization}

    Normalize values using approach by Markovic (2010), see equation bellow. Main benefit is that the normalization works bor both benefit and cost criteria.

    \mjsdeqn{z = 1 - \frac{x - min(x)}{max(x)}}

    \bold{Min-max method}

    Min-max normalization is one of most used normalization methods. It takes values in any ascale an normalizes them into 0-1 scale.

    Normalized values z are using computed for benefit criteria:

    \mjsdeqn{z = \frac{x - min(x)}{max(x) - min(x)}}

    For cost criteria the computation returns 1 - z, where z is computed using equation above.

    \bold{Nonlinear normalization}

    Normalize values using approach by Oeldschus et al (1983), see equations bellow.

    For benefit criteria

    \mjsdeqn{z = (\frac{x}{max(x)})^2}

    For cost criteria

    \mjsdeqn{z = (\frac{max(x)}{x})^2}

    \bold{Normalize to average value}

    Normalized values are computed by comparing the values to average.

    \mjsdeqn{z = \frac{x}{\mu} \cdot 100}

    \bold{Normalize to best value}

    Normalized values are computed by comparing the values to maximum. Results show how close the values are to this maximum (in percents).

    \mjsdeqn{z = \frac{x}{max(x)} \cdot 100}

    \bold{Normalize values using Tzeng and Huang approach}

    Normalize values using approach by Tzeng and Huang (2011), see equation bellow. Main benefit is that the normalization works bor both benefit and cost criteria.

    \mjsdeqn{z = \frac{max(x)}{x}}

    \bold{Vector normalization}

    Normalized values are computed as follows.

    For maximization transformation

    \mjsdeqn{z = \frac{x}{\sqrt{\sum_{i=1}^m x_i^2}}}

    For minimization transformation

    \mjsdeqn{z = \frac{\frac{1}{x}}{\sqrt{\sum_{i=1}^m \frac{1}{x_i^2}}}}

    where m is number of observation in vector to normalize

    \bold{Normalize values using Zavadskas and Turskis approach}

    ormalize values using approach by Zavadskas and Turskis (2008), see equations bellow.

    For benefit criteria

    \mjsdeqn{z = 1 - |\frac{max(x) - x}{max(x)}|}

    For cost criteria

    \mjsdeqn{z = 1 - |\frac{min(x) - x}{min(x)}|}

    \bold{Z-Score - standard score}

    Standard score is the number of standard deviations by which the value of raw score is above or below mean value. Values > mean will have positive scores, while values under mean will be negative.
Normalized values z are computed:

    \mjsdeqn{z = \frac{x - \mu}{\sigma}}
}

\usage{
  normalize(tonorm, minmax = "max", method = "minmax")
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
  \item{minmax}{'min' or 'max' to specify cost or benefit criterion, max is default value}
  \item{method}{specify normalization method, use constant from table. 'minmax' is default value.}
}

\value{
  returns normalized vector of values.
}

\references{
    Lai, Young-hou & Ching-Lai Hwang (1994). Fuzzy Multiple Objective Decision Making Method and Applications. In: Lecture Notes in Economics and Mathematical Systems 404. Berlin, Heidelberg: Springer-Verlag. DOI: 10.007/978-3-642-57949-3.

    MARKOVIC, Z. Modification of TOPSIS Method For Solving Multicriteria Tasks. Yugoslav Journal of Operations Researc, vol. 20, no. 1, pp. 117-143, DOI: 10.2298/YJOR1001117M

    Normalization. Code academy, available from \url{https://www.codeacademy.com/articles/normalization} [cit. 2021-09-28]

    Peldschus, F., Vaigauskas, E. and Zavadskas, E.K. (1983) Technologische Entscheidungen bei der Berücksichtigung mehrerer Ziele. Bauplanung - Bautechnik, vol. 37, no. 4, pp. 173-175.

    Tzeng, G. and Huang, J. (2011) Multiple Attribute Decision Making Methods and Applications. CRC Press, Taylor and Francis Group, A Chapman & Hall Book, Boca Raton. 350 p., ISBN 978-1-4398-6157-8

    Zavadskas, E.K., Turskis, Z. (2008). A New Logarithmic Normalization Method in Games Theory. Informatica, vol. 19, no. 2, pp. 303-314
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
    # normalize 1,2,3 vector representing benefit criterion using
    # method for nonlinear normalization
    tonorm <- c(1,2,3)
    normalized <- mcda_norm(tonorm, minmax = "max", method ="nonlinear")

    # compute Z-score for same vector
    scaled <- mcda_norm(tonorm, method = "zscore")
}

\keyword{normalization}
\keyword{Lai-Hwang}
\keyword{linear aggregation}
\keyword{logarithmic normalization}
\keyword{Markovic normalization}
\keyword{min-max}
\keyword{nonlinear normalization}
\keyword{normalization toaverage}
\keyword{normalization tobest}
\keyword{Tzeng-Huang normalization}
\keyword{vector normalization}
\keyword{Zavadskas-Turskis normalization}
\keyword{Z-score}