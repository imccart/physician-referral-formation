# Paper Review: Comments and Suggestions

## 1. Grammatical / Exposition-Level Edits

**(i) Tighten repetition in the introduction**
- The introduction reiterates the importance of referrals across multiple consecutive sentences.
- Suggest collapsing into a single sharper statement to improve pacing before presenting the contribution.

**(ii) Clarify language around “causal”**
- The phrase “some of the first causal evidence” may be too strong given the design.
- Consider softening to: “credible causal evidence” or “quasi-experimental evidence.”

**(iii) Equation exposition**
- The transition from latent utility to the logit form is somewhat abrupt.
- Add a sentence clarifying that the Type I extreme value assumption yields the logistic formulation.

**(iv) Notation consistency**
- The shift from \(a_{ij}\) to \(\alpha_i + \alpha_j - g(\xi_i, \xi_j)\) is not explicitly restated.
- Add a brief reminder that this is a parameterization of \(a_{ij}\).

**(v) Repetitive phrasing**
- Phrases like “this pattern suggests…” are used frequently.
- Vary with alternatives such as “consistent with…” or “suggesting that…”.

**(vi) Appendix tone (race prediction section)**
- The appendix reads more like a technical report.
- Streamline by focusing on accuracy, tradeoffs, and implications for measurement error rather than detailed method descriptions.

---

## 2. Content / Structural Suggestions

**(i) Identification: sharpen the key assumption**
- The assumption that PCPs do not relocate to refer to specific specialists is central but underdeveloped.
- Add discussion of:
  - Why the assumption is plausible
  - What violations would look like
  - Likely direction of bias

**(ii) Mechanism interpretation: practice affiliation**
- Current interpretation focuses on “organizational convenience.”
- Expand to distinguish between:
  1. Information (familiarity with colleagues)
  2. Incentives (financial integration)
  3. Constraints (internal referral norms or policies)

**(iii) Dynamics section: sharpen interpretation**
- The attenuation result is strong but could be framed more clearly.
- Consider structuring as:
  - Phase 1: Default heuristic (organizational + proximity)
  - Phase 2: Learning and updating
  - Phase 3: Steady-state network

**(iv) Cross-specialty comparison**
- Currently reads as summary rather than synthesis.
- Introduce a unifying concept such as “institutional dependence of referral frictions.”
- Map differences across specialties to institutional features.

**(v) Quality section positioning**
- The quality analysis feels somewhat detached.
- Options:
  - Expand and integrate earlier in the paper, or
  - Explicitly frame as a back-of-the-envelope exercise

**(vi) Appendix: separation / non-convergence**
- Important methodological detail is buried.
- Add a brief mention in the main text noting reliance on the non-separated subsample and stability of marginal effects.

---

## 3. High-Value New Analyses (Ceiling Improvements)

### 1. Heterogeneity by specialist quality
- Interact key covariates (practice affiliation, distance, gender concordance) with specialist quality.
- Alternatively, estimate models separately by quality terciles.

**Goal:**
- Determine whether heuristics are more pronounced when referring to lower-quality specialists.

---

### 2. Within-practice vs. outside-practice decision
- Model referral formation as a two-step process:
  1. Choose inside vs. outside practice
  2. Choose specialist conditional on that decision

**Benefit:**
- Clarifies mechanisms and reduces dominance of practice affiliation in a single equation.

---

### 3. Learning / information accumulation
- Test whether attenuation varies with market characteristics.

**Approaches:**
- Interact time since move with:
  - Specialist density
  - Market size
- Use proxies for prior exposure (e.g., training location if available)

---

### 4. Patient-level consequences
- Examine downstream outcomes such as:
  - Distance traveled by patients
  - Fragmentation (number of specialists seen)

**Purpose:**
- Extend policy relevance beyond physician behavior to patient experience.

---

### 5. Network persistence / path dependence
- Analyze persistence of initial referral links over time.

**Suggested analysis:**
- Estimate probability a first-year link persists as a function of:
  - Practice affiliation
  - Distance
  - Demographic concordance

---

## Bottom Line

The paper is already strong in design and execution. The main gains are:
- Sharpening identification and mechanism discussion
- Framing dynamics more explicitly as a behavioral process
- Adding targeted heterogeneity or learning-based analyses

These changes would strengthen both conceptual contribution and policy relevance.

