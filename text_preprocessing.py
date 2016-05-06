f = open('full_contents.txt')
data = f.read()
f.close()

doc = data.split('***')

f = open('table_of_contents.txt')
table = f.read()
f.close()

table_lines = table.split('\n')

years = []
for i in range(len(table_lines)) :
    line = table_lines[i].split(',')
    if line[-1] != "" :
        years.append( line[-1].strip() + '_' + line[-2].strip().split(' ')[0] + '_' + line[-2].strip().split(' ')[1])

## outputing text files
for i in range(len(doc)) : 
    file = open(years[i] + ".txt", "w")
    file.write(doc[i])
    file.close()

years_csv = []
months_csv = []
names_csv = []
for i in range(len(table_lines)) :
    line = table_lines[i].split(',')
    if line[-1] != "" :
        years_csv.append( line[-1].strip() )
        months_csv.append( line[-2].strip().split(' ')[0] )
        names_csv.append( line[0] )

## outputing name - year correspondence
import csv as csv

output_file = open("name_year_month.csv", "wb")
output_file_object = csv.writer(output_file)
output_file_object.writerow(["Name","Year", "Month"])
output_file_object.writerows(zip(names_csv, years_csv, months_csv))
output_file.close()


