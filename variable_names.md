# Simulation Function Structure

---

## 🔹 `simulate`  
- acts as trigger to simulate the Solow model  
- (it is what is simulated when user selects **simulate**)  
- `simulate_solow` runs the calculations (experiments applied already)

---

## 🔹 Simulate Experiments Trigger  
this is the function that triggers `simulate`  
(`simulate` triggers `simulate_solow`)

---

## 🔹 Simulate Experiment Function Names

- `simulate_first_exp_calculations`  
- `simulate_counter_exp_calculations`  
- `simulate_second_exp_calculations`  
- `simulate_third_exp_calculations`  
- `simulate_fourth_exp_calculations`

---

## 🔹 Experiment Results: Data Frame Names

- `results_first_exp_df`  
- `results_counterfactual_df`  
- `results_second_exp_df`  
- `results_third_exp_df`  
- `results_fourth_exp_df`

---

## 🔹 Experiments Visuals  
anything related to df visuals (plots, ui)

- `show_counter_visual`  
- `show_second_exp_visual`  
- `show_third_exp_visual`  
- `show_fourth_exp_visual`

---

## 🔹 Plot Function  
plot code that serves as the callable variable for plot code and connection between plot functions and simulation functions (`simulate_..._exp_calculations`)

---

## 🔹 Placeholders for the `make_plot` Code  
this will be assigned the assigned df in the `make_plot` function

- `show_plot_placeholder_first`  
- `show_plot_placeholder_counter`  
- `show_plot_placeholder_second`  
- `show_plot_placeholder_third`  
- `show_plot_placeholder_fourth`

---

## ➕ Adding Plot

1. add plot id to ui for app display  
2. in `plot_specs` add plot names: id, x and y axis name and title  

that is it, everything else is loop.  
**not updated, so maybe one section is not on here.**

---

## 🧠 Notes

I made this model by making individual data base and individual calculations for each new thing I added. I did this because I found it easier to check the model and make sure it work. Having said that this loops can be shorten, specially the ones that trigger the simulation.

