#'
#' Generate descriptive summary for objects returned by other functions.
#'
#' @param object the object returned by other functions.
#' @param ... ignored arguments
#'
#'
#' @details \code{summary()} prints the objects returned by other functions.
#'
#' @return \code{summary()} prints the objects returned by other functions.
#'
#' @author Suyu Liu, Liangcai Zhang and Ying Yuan
#'
#' @examples
#' ## get the operating characteristics for BOIN design
#' S1.p.true <- matrix(c(0.02, 0.07, 0.10,
#' 0.07, 0.10, 0.15,
#' 0.10, 0.15, 0.30,
#' 0.15, 0.30, 0.45,
#' 0.30, 0.45, 0.55), nrow = 3, byrow = FALSE)

#' doses.cp <- matrix(c(1, -1, -1, -1, -1,
#'       1, 1, 1, 1, -1,
#'       1, 1, 1, 1, 1),
#'       ncol = 5, byrow = TRUE)
#' oc.comb <- get.oc.combP(target=0.3, p.true=S1.p.true, ncohort=10, cohortsize=3,
#'                                preferred.doses = doses.cp, n.earlystop=9, startdose=c(1,1), ntrial=1000)
#'
#' summary(oc.comb)
#'
#'
#' @import BOIN
#' @export

summary.boinP<- function (object, ...)
{
  if (!is.null(object$boundary_tab)) {
    if (!is.na(object$lambda_e))
      cat("Escalate dose if the observed DLT rate at the current dose <= ",
          object$lambda_e, "\n")
    if (!is.na(object$lambda_d))
      cat("Deescalate dose if the observed DLT rate at the current dose > ",
          object$lambda_d, "\n\n")
    if (!is.null(object$boundary_tab)) {
      cat("This is equivalent to the following decision boundaries\n")
      print(object$boundary_tab)
    }
    if (!is.null(object$full_boundary_tab)) {
      cat("\n")
      cat("A more completed version of the decision boundaries is given by\n")
      print(object$full_boundary_tab)
    }
    if (!is.null(object$stop_boundary)) {
      cat("\n")
      cat("In addition to the default stopping rule (i.e., stop the trial if the lowest dose is eliminated), \n")
      cat("the following more strict stopping safety rule will be used for extra safety: \n")
      cat(" stop the trial if (1) the number of patients treated at the lowest dose >= 3 AND",
          "\n", "(2) Pr(the DLT rate of the lowest dose >",
          object$target, "| data) > ", object$cutoff, ",\n",
          "which corresponds to the following stopping boundaries:\n")
      print(object$stop_boundary)
    }
    else {
      cat("\n")
      cat("Default stopping rule: stop the trial if the lowest dose is eliminated.\n")
    }
  }
  if (!is.null(object$next_subtrial)) {
    if (is.na(object$next_subtrial) == TRUE) {
      cat("No additional next subtrials are needed!!\n")
    }
    else {
      cat("Next subtrial includes doses: ", "\n")
      cat("\t\t", object$next_subtrial, "\n\n")
      cat("The starting dose for this subtrial is:\n",
          "\t\t", paste("(", object$starting_dose[1], ", ",
                        object$starting_dose[2], ")", sep = ""), "\n")
    }
  }
  if (!is.null(object$next_dc)) {
    if (is.na(object$next_dc[1]) == TRUE) {
      cat("The trial experienced an early stopping.")
    }
    else {
      cat("The recommended dose combination for the next cohort of patients is (",
          object$next_dc[1], ", ", object$next_dc[2], ").",
          "\n")
    }
  }
  if (!is.null(object$MTD)) {
    if (length(object$MTD) == 1) {
      if (object$MTD == 99) {
        cat("All tested doses are overly toxic. No MTD should be selected! \n\n")
      }
      else {
        cat("The MTD is dose level ", object$MTD, "\n\n")
      }
      cat("Dose    Posterior DLT             95%                  \n",
          sep = "")
      cat("Level     Estimate         Credible Interval   Pr(toxicity>",
          object$target, "|data)\n", sep = "")
      for (i in 1:nrow(object$p_est)) {
        cat(" ", i, "        ", as.character(object$p_est[i,
                                                          2]), "         ", as.character(object$p_est[i,
                                                                                                      3]), "         ", as.character(object$p_overdose[i]),
            "\n")
      }
      cat("NOTE: no estimate is provided for the doses at which no patient was treated.\n")
    }
    if (length(object$MTD) >= 2) {
      if (length(object$MTD) == 2) {
        if (object$MTD[1, 1] == 99 && object$MTD[1, 2] ==
            99) {
          cat("All tested doses are overly toxic. No MTD is selected! \n")
        }
        else cat("The MTD is dose combination (", object$MTD[1,
                                                             1], ", ", object$MTD[1, 2], ") \n\n")
      }
      else {
        if (length(object$MTD) == 0) {
          cat("All tested doses are overly toxic. No MTD is selected! \n")
        }
        else {
          cat("The MTD contour includes dose combinations ",
              paste("(", object$MTD[, 1], ", ", object$MTD[,
                                                           2], ")", sep = ""), "\n\n")
        }
      }
      cat("Isotonic estimates of toxicity probabilities and 95% confidence intervals for combinations are \n")
      # for (i in 1:dim(object$p_est_CI)[1]) {
      #   cat(formatC(object$p_est_CI[i, ], digits = 2, format = "f",
      #               width = 5), sep = "  ", "\n")
      # }
      print(noquote(object$p_est_CI))
      cat("\n")
      cat("NOTE: no estimate is provided for the doses at which no patient was treated.\n\n")
    }
  }
  if (!is.null(object$percentstop)) {
    if (!is.null(object$overdose60)) {
      cat("selection percentage at each dose level (%):\n")
      cat(formatC(object$selpercent, digits = 1, format = "f"),
          sep = "  ", "\n")
      cat("average number of patients treated at each dose level:\n")
      cat(formatC(object$npatients, digits = 1, format = "f"),
          sep = "  ", "\n")
      cat("average number of toxicity observed at each dose level:\n")
      cat(formatC(object$ntox, digits = 1, format = "f"),
          sep = "  ", "\n")
      cat("average number of toxicities:", formatC(object$totaltox,
                                                   digits = 1, format = "f"), "\n")
      cat("average number of patients:", formatC(object$totaln,
                                                 digits = 1, format = "f"), "\n")
      cat("percentage of early stopping due to toxicity:",
          formatC(object$percentstop, digits = 1, format = "f"),
          "% \n")
      cat("risk of overdosing (>50% of patients treated above the MTD):",
          formatC(object$overdose50, digits = 1, format = "f"),
          "% \n")
      cat("risk of overdosing (>60% of patients treated above the MTD):",
          formatC(object$overdose60, digits = 1, format = "f"),
          "% \n")
      cat("risk of overdosing (>80% of patients treated above the MTD):",
          formatC(object$overdose80, digits = 1, format = "f"),
          "% \n")
    }
    else {
      cat("selection percentage at each dose level (%):\n")
      cat(formatC(object$selpercent, digits = 1, format = "f"),
          sep = "  ", "\n")
      cat("average number of patients treated at each dose level:\n")
      cat(formatC(object$npatients, digits = 1, format = "f"),
          sep = "  ", "\n")
      cat("average number of toxicity observed at each dose level:\n")
      cat(formatC(object$ntox, digits = 1, format = "f"),
          sep = "  ", "\n")
      cat("average number of toxicities:", formatC(object$totaltox,
                                                   digits = 1, format = "f"), "\n")
      cat("average number of patients:", formatC(object$totaln,
                                                 digits = 1, format = "f"), "\n")
      cat("percentage of early stopping due to toxicity:",
          formatC(object$percentstop, digits = 1, format = "f"),
          "% \n")
    }
  }
  if (!is.null(object$npercent) | !is.null(object$npercent.contour)) {
    if (!is.null(object$npercent.contour)) {
      cat("true DLT rate of dose combinations:\n")
      for (i in 1:dim(object$p.true)[1]) cat(formatC(object$p.true[i,
      ], digits = 2, format = "f", width = 5), sep = "  ",
      "\n")
      cat("\n")
      cat("selection percentage at each dose combination (%):\n")
      for (i in 1:dim(object$p.true)[1]) cat(formatC(object$selpercent[i,
      ], digits = 2, format = "f", width = 5), sep = "  ",
      "\n")
      cat("\n")
      cat("average number of patients treated at each dose combination:\n")
      for (i in 1:dim(object$p.true)[1]) cat(formatC(object$npatients[i,
      ], digits = 2, format = "f", width = 5), sep = "  ",
      "\n")
      cat("\n")
      cat("average number of toxicity observed at each dose combination:\n")
      for (i in 1:dim(object$p.true)[1]) cat(formatC(object$ntox[i,
      ], digits = 2, format = "f", width = 5), sep = "  ",
      "\n")
      cat("\n")
      cat("average number of toxicities:", formatC(object$totaltox,
                                                   digits = 1, format = "f"), "\n")
      cat("average number of patients:", formatC(object$totaln,
                                                 digits = 1, format = "f"), "\n")
      cat("percentage of patients treated at MTD contour:",
          object$npercent.contour, "\n")
      cat("percentage of patients treated above MTD contour:",
          formatC(object$npercent.above.contour, digits = 1,
                  format = "f"), "\n")
      cat("percentage of patients treated below MTD contour:",
          formatC(object$npercent.below.contour, digits = 1,
                  format = "f"), "\n")
      cat("percentage of correct selection of the MTD contour:",
          formatC(object$pcs.contour, digits = 1, format = "f"),
          "\n")
    }
    else {
      cat("true DLT rate of dose combinations:\n")
      for (i in 1:dim(object$p.true)[1]) {
        cat(formatC(object$p.true[i, ], digits = 2, format = "f",
                    width = 5), sep = "  ", "\n")
      }
      cat("\n")
      cat("Preferred dose combinations:\n")
      for (i in 1:dim(object$p.true)[1]) cat(formatC(object$preferred.doses[i,
      ],  format = "f", width = 6), sep = "  ",
      "\n")
      cat("\n")
      cat("selection percentage at each dose combination (%):\n")
      for (i in 1:dim(object$p.true)[1]) {
        cat(formatC(object$selpercent[i, ], digits = 2,
                    format = "f", width = 5), sep = "  ", "\n")
      }
      cat("\n")
      cat("average number of patients treated at each dose combination:\n")
      for (i in 1:dim(object$p.true)[1]) {
        cat(formatC(object$npatients[i, ], digits = 2,
                    format = "f", width = 5), sep = "  ", "\n")
      }
      cat("\n")
      cat("average number of toxicity observed at each dose combination:\n")
      for (i in 1:dim(object$p.true)[1]) {
        cat(formatC(object$ntox[i, ], digits = 2, format = "f",
                    width = 5), sep = "  ", "\n")
      }
      cat("\n")
      cat("average number of toxicities:", formatC(object$totaltox,
                                                   digits = 1, format = "f"), "\n")
      cat("average number of patients:", formatC(object$totaln,
                                                 digits = 1, format = "f"), "\n")
      # cat("selection percentage of MTD:", formatC(object$pcs,
      #                                             digits = 1, format = "f"), "\n")
      # cat("percentage of patients treated at MTD:", formatC(object$npercent,
      #                                                       digits = 1, format = "f"), "\n")
      cat("percentage of early stopping due to toxicity:",
          formatC(object$percentstop, digits = 1, format = "f"),
          "% \n")
    }
  }
}
