
## Exploring the rapeseed (*Brassica napus* L.) genomic variation dataset
<p>&nbsp;</p>
<p>Rapeseed (<em>Brassica napus</em> L.), an important oilseed crop, has adapted to diverse climate zones and latitudes by forming three main ecotype groups, namely winyer, semi-winter, and spring types. In this database, We collected a worldwide collection of 1007 <em>B .napus</em> germplasms accessions, including 658 winetr types, 145 semi-winter types, and 188 spring types, from 39 countries (Fig. 1).</p>

<p>&nbsp;</p>
<p align="center">
<img src="img/accession_distribution.png" width="800" hegiht="1000">
</p>
<p style="text-align:center">Fig. 1 The geographic distribution of rapeseed accessions </p>

<p>We generated a total of 7.82 Tb of clean reads from whole-genome resequencing of the world collection from 39 countries across the world with an average of ~6.6-fold-coverage. We used the following pipeline (Fig. 2) to process the data:</p>

<p>&nbsp;</p>

<p align="center">
<img src="img/pipeline.png" width="300" hegiht="400">
</p>
<p style="text-align:center">Fig. 2 The pipeline for data processing procedures </p>

1. All raw paired-end reads were trimmed for quality control using Trimmomatic
2. The remaining high-quality reads were aligned to the genome of Darmor-*bzh* with BWA software
3. BAM alignments files were sorted and duplicated reads were marked subsequently generated with SAMtools and Picard
4. SNPs were identified and filtering using GATK. SNPs annotation was performed using SnpEFF software.

<p>&nbsp;</p>


<p>Finally, 5.56 million SNPs were detected. The distribution of polymorphisms in <em>B. napus</em> genomic regions showed that 202 323, 62 834, 939 553, 899 982, and 2 778 189 SNPs were located within exons, introns, and upstream regions (within 5 kb upstream of transcription start sites), downstream regions (within 5 kb downstream of transcription stop sites), and intergenic regions, respectively (Fig. 3).


<p>&nbsp;</p>
<p align="center">
<img src="img/variations_distribution.png" width="500" hegiht="500">
</p>
<p style="text-align:center">Fig. 3 The distribution of polymorphisms in <em>B. napus</em> genomic regions </p>






