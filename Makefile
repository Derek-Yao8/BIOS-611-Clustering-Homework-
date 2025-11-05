RSCRIPT := Rscript --vanilla
FIGDIR  := figures

TASK1   := Task1.R
TASK2   := Task2.R

# Figures to build 
FIG1    := $(FIGDIR)/Gap_Statistic_Plots.png
FIG2    := $(FIGDIR)/Spectral_clustering_plot.png

FIGS    := $(FIG1) $(FIG2)

.PHONY: all figures clean

# Default: build all figures
all: figures

figures: $(FIGS)

# Ensure figures/ exists (order-only prerequisite)
$(FIGDIR):
	@mkdir -p $(FIGDIR)

# Each script builds its own figure (pass output path)
$(FIG1): $(TASK1) | $(FIGDIR)
	@echo ">> Running $(TASK1) -> $@"
	$(RSCRIPT) $(TASK1) --out $@

$(FIG2): $(TASK2) | $(FIGDIR)
	@echo ">> Running $(TASK2) -> $@"
	$(RSCRIPT) $(TASK2) --out $@

# Clean up generated figures
clean:
	@rm -rf $(FIGDIR)