# Find all figures in supplement.
cd /nfs/qread-data/virtualland/Rmd
grep "fpfig" PNAS_SI.Rmd | grep "centerline" > figs.txt

# Extract the file paths from those lines.
sed -i 's&\\centerline{\\includegraphics\[width\=0.8\\textwidth\]{\\fpfig/&&g' figs.txt
sed -i 's&\\centerline{\\includegraphics\[width\=1.0\\textwidth\]{\\fpfig/&&g' figs.txt
sed -i 's&}}&&g' figs.txt

# Convert the pngs to pdfs in place.
fpfig='/nfs/qread-data/cfs_io_analysis/scenario_v2_figs'

while read pngfile; do
  pdffile=${pngfile/png/pdf}
  convert -quality 100 $fpfig/$pngfile $fpfig/$pdffile
done < figs.txt