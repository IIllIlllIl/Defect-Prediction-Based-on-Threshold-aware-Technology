import csv

'''
Dataset Reading
param: 
    route -> file path
    
return:
    X -> the metrics of the dataset
    Y -> the label of the dataset
'''
def data_reading(route):
    X = []
    Y = []
    with open(route) as csvfile:
        csv_reader = csv.reader(csvfile)
        header = next(csv_reader)
        for row in csv_reader:
            if "JURECZKO" in route:
                row = [row[0]] + row[4:]
            f_row = list(map(lambda x: float(x), row))
            X.append(f_row[:-1])
            Y.append(f_row[-1])
    return X, Y