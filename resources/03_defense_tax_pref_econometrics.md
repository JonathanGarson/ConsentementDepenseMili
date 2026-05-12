# Econometrics in `03_defense_tax_pref.R`

This script estimates descriptive associations between respondent characteristics
and preferred ways to finance higher defence spending. It does not estimate a
causal treatment effect.

## Outcome and Sample

The outcome comes from q36, a multiple-response question with four possible
funding options:

- `q36_q1`: lower other public spending
- `q36_q2`: higher taxes or social contributions
- `q36_q3`: higher deficit and debt
- `q36_q4`: more hours or days worked

The script keeps q36 as a multiple-response outcome. It reshapes the survey to a
long respondent-option file: each respondent contributes four rows, one for each
funding option. The dependent variable is `selected_ij`, equal to 1 when
respondent `i` selected option `j`, and 0 otherwise. Respondents with no q36
selection and respondents with several q36 selections are both retained if they
otherwise satisfy the model sample restrictions.

The estimation sample is restricted to observations with `treatment == 0`, valid
positive survey weights, `q5` equal to `Oui` or `Non`, valid 0/1 q36 indicators,
and complete cases for the q36 indicators plus all model covariates. Age is
centered and scaled using the weighted mean and weighted standard deviation in
the estimation sample; its square is then included.

## Model

The model is a weighted mixed binary probit estimated with `lme4::glmer()`:

```text
Pr(selected_ij = 1 | X_i, Z_i, u_i)
  = Phi(alpha_j + X_i' beta_j + Z_i' gamma + u_i)
```

where `Phi(.)` is the standard normal CDF. `alpha_j` is an option-specific
intercept. `X_i` contains the focal covariates whose coefficients are allowed to
vary by funding option through interactions with `funding_option`. `Z_i`
contains additional controls whose coefficients are common across funding
options. `u_i` is a respondent random intercept, capturing persistent
respondent-level propensity to select q36 options and the dependence among the
four option rows for the same respondent.

The implemented fixed-effect formula is:

```text
selected ~ funding_option * (
  q30_q2 + q35 + q19 + q5 + continuous_age_scaled +
  continuous_age_scaled_sq + acte_citoyen + q28 + gender +
  tranche_revenus
) +
  pcs + matri + foyer + tailleagglo5 + diplome + partisane + sat
```

with random effect `(1 | respid)` and a binomial probit link. The focal
interacted covariates are trust in the state, perceived conflict risk, perceived
tax level, income-tax payer status, scaled age, scaled age squared, tax as a
civic act, satisfaction with public money use, gender, and income bracket. The
additional controls are socioeconomic status, marital status, household
composition, urban-area size, education, partisan proximity, and general
satisfaction.

Because the four funding options are modeled as four binary responses, this is
not a multinomial choice model. The probabilities are not constrained to sum to
one across options, which matches the multiple-response structure of q36.

## Reported Effects

The exported table reports option-specific average marginal effects (AMEs), not
latent-index probit coefficients. For each funding option, the AME of a factor
level is computed by predicting each observation twice, once at the reference
level and once at the comparison level, then averaging the probability
difference with normalized survey weights. For continuous variables, the AME is
a finite-difference derivative using a small increment.

Predictions set the respondent random effect to zero, so the AMEs describe
changes in the fixed-effect probability for an average random-effect
respondent. Standard errors use the delta method with the fixed-effect
variance-covariance matrix from the fitted GLMM.

Survey weights enter `glmer()` as likelihood weights and enter the AME averages
as normalized respondent weights. They are not a full survey-design correction:
the script does not account for strata, primary sampling units, calibration
uncertainty, or design-based clustered inference. The model is estimated with
the fast `nAGQ = 0` approximation.

The main outputs are:

- `output/tables/defense_tax_pref_ame_q36_glmm.tex`
- `output/tables/defense_tax_pref_ame_q36_glmm.html`

