library(ggplot2)
library(grid)
library(gridExtra)

library(datasets); data(mtcars);library(knitr)

mtcars$am <- factor(mtcars$am, levels = c(0, 1), 
                    labels = c("Automatic", "Manual"))
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$vs     <- as.factor(mtcars$vs)

# Exploratory Analysis
# - Basically it could go either way

xlab <- "Transmission Type"
ylab <- "Miles Per Gallon (MPG)"
g <- ggplot(data = mtcars, aes(x = factor(am), y = mpg, fill = am)) + 
        scale_fill_discrete(name = xlab) +
        xlab(xlab) + 
        ylab(ylab) +
        geom_violin(col = "black", size = 1.5)
print(g)

single_fit <- lm(mpg ~ am, mtcars)

summary(single_fit)$coefficients


#Model Selection
fit_all <- glm(mpg ~ ., data = mtcars)
fit_wt <- lm(mpg ~ wt, mtcars)
stepfit <- step(fit_all, trace = 0)

# ggplot regression code from http://susanejohnston.wordpress.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/

ggplotRegression <- function (fit) {
        
        require(ggplot2)
        
        ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
                geom_point() +
                stat_smooth(method = "lm", col = "red") +
                ggtitle(paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                                   "; Intercept =",signif(fit$coef[[1]],5 ),
                                   "; Slope =",signif(fit$coef[[2]], 5),
                                   "; P =",signif(summary(fit)$coef[2,4], 5)))
}

# This program is released under the GNU GPL >=2 license.  For Details, see http://librestats.com/licenses

autoplot.lm <- function(x, ..., which=c(1:3, 5), mfrow=c(1,1)){
        
        require(grid)
        df <- fortify(x)
        df <- cbind(df, rows=1:nrow(df))
        
        # residuals vs fitted
        g1 <- ggplot(df, aes(.fitted, .resid)) +
                geom_point()  +
                geom_smooth(se=FALSE) +
                geom_hline(linetype=2, size=.2) +
                scale_x_continuous("Fitted Values") +
                scale_y_continuous("Residual") +
                ggtitle("Residuals vs Fitted")
        
        # normal qq
        a <- quantile(df$.stdresid, c(0.25, 0.75))
        b <- qnorm(c(0.25, 0.75))
        slope <- diff(a)/diff(b)
        int <- a[1] - slope * b[1]
        g2 <- ggplot(df, aes(sample=.resid)) +
                stat_qq() +
                geom_abline(slope=slope, intercept=int) +
                scale_x_continuous("Theoretical Quantiles") +
                scale_y_continuous("Standardized Residuals") +
                ggtitle("Normal Q-Q")
        
        # scale-location
        g3 <- ggplot(df, aes(.fitted, sqrt(abs(.stdresid)))) +
                geom_point() +
                geom_smooth(se=FALSE) +
                scale_x_continuous("Fitted Values") +
                scale_y_continuous("Root of Standardized Residuals") +
                ggtitle("Scale-Location")
        
        # cook's distance
        g4 <-  ggplot(df, aes(rows, .cooksd, ymin=0, ymax=.cooksd)) +
                geom_point() + geom_linerange() +
                scale_x_continuous("Observation Number") +
                scale_y_continuous("Cook's distance") +
                ggtitle("Cook's Distance")
        
        # residuals vs leverage
        g5 <- ggplot(df, aes(.hat, .stdresid)) +
                geom_point() +
                geom_smooth(se=FALSE) +
                geom_hline(linetype=2, size=.2) +
                scale_x_continuous("Leverage") +
                scale_y_continuous("Standardized Residuals") +
                ggtitle("Residuals vs Leverage")
        
        # cooksd vs leverage
        g6 <- ggplot(df, aes(.hat, .cooksd)) +
                geom_point() +
                geom_smooth(se=FALSE) +
                scale_x_continuous("Leverage") +
                scale_y_continuous("Cook's distance") +
                ggtitle("Cook's dist vs Leverage")
        
        plots <- list(g1, g2, g3, g4, g5, g6)
        
        # making the plots
        grid.newpage()
        
        if (prod(mfrow)>1) {
                mypos <- expand.grid(1:mfrow[1], 1:mfrow[2])
                mypos <- mypos[with(mypos, order(Var1)), ]
                pushViewport(viewport(layout = grid.layout(mfrow[1], mfrow[2])))
                formatter <- function(.){}
        } else {
                mypos <- data.frame(matrix(1, length(which), 2))
                pushViewport(viewport(layout = grid.layout(1, 1)))
                formatter <- function(.) {
                        .dontcare <- readline("Hit <Return> to see next plot: ")
                        grid.newpage()
                }
        }
        
        j <- 1
        for (i in which){
                formatter()
                print(plots[[i]], vp=viewport(layout.pos.row=mypos[j,][1], layout.pos.col=mypos[j,][2]))
                j <- j+1
        }
}

autoplot(fit_wt, which=1:6, mfrow=c(3,2))