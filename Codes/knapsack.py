# -*- coding: utf-8 -*-
"""
Spyder Editor
6
This is a temporary script file.
"""

from ortools.linear_solver import pywraplp



class item:
    def __init__ (self, cont_id, company):
        self.cont_id = cont_id
        self.company = company
    
class Cdr():

    def __iter__(self):
        return iter([self.cont_id, self.company])
    

def create_data_model():
    """Create the data for the example."""
    new_list = []
    weights = []
    values = []
    jobs = int(input("Enter the number of the jobs \n"))
    input2=input()
    for line in input2.splitlines():
        id, c_id, hours = line.split()
        hours = float(hours)
        id=int(id)
        halfHours = hours*2
        halfHours = int(halfHours)
        #c_id = input("Enter company Name: \n")
        new_list.append(item(id, c_id))
        #hours = int(input("Enter no. of hours\n"))
        weights.append(halfHours)
        values.append(halfHours)


    data = {}
    data['weights'] = weights
    data['values'] = values
    data['items'] = list(range(len(weights)))
    data['num_items'] = len(weights)
    num_bins = 8
    data['bins'] = list(range(num_bins))
    data['bin_capacities'] = [48, 48, 48, 48, 48, 48, 48, 48]
    return data, weights, new_list


def main():
    data,weights,new_list = create_data_model()
    

    # Create the mip solver with the SCIP backend.
    solver = pywraplp.Solver.CreateSolver('SCIP')

    # Variables
    # x[i, j] = 1 if item i is packed in bin j.
    x = {}
    for i in data['items']:
        for j in data['bins']:
            x[(i, j)] = solver.IntVar(0, 1, 'x_%i_%i' % (i, j))

    # Constraints
    # Each item can be in at most one bin.
    for i in data['items']:
        solver.Add(sum(x[i, j] for j in data['bins']) <= 1)
    # The amount packed in each bin cannot exceed its capacity.
    for j in data['bins']:
        solver.Add(
            sum(x[(i, j)] * data['weights'][i]
                for i in data['items']) <= data['bin_capacities'][j])

    # Objective
    objective = solver.Objective()

    for i in data['items']:
        for j in data['bins']:
            objective.SetCoefficient(x[(i, j)], data['values'][i])
    objective.SetMaximization()

    status = solver.Solve()
    nestedlist = []

    if status == pywraplp.Solver.OPTIMAL:
        print('Total packed value:', objective.Value())
        total_weight = 0
        for j in data['bins']:
            bin_weight = 0
            bin_value = 0
            burn_list = []
            print('Bin ', j, '\n')
            for i in data['items']:

                if x[i, j].solution_value() > 0:
                    #print('Item', i, '- weight:', data['weights'][i], ' value:',data['values'][i])
                    burn_list.append(i)

                    bin_weight += data['weights'][i]
                    bin_value += data['values'][i]
            print('Packed bin weight:', bin_weight)
            print('Packed bin value:', bin_value)
            nestedlist.append(burn_list)
            print()
            total_weight += bin_weight
        print('Total packed weight:', total_weight)
    else:
        print('The problem does not have an optimal solution.')
    

    print(nestedlist)
    matrix = [[-1 for x in range (8)] for y in range (48)]
    counter = -1
    for lot_list in nestedlist:
        counter+= 1
        z = 0
        for item in lot_list:
            halfHours = weights[item]
            print("hours is : ", halfHours/2)
            print("item is: ", item)
            
            for k in range(halfHours):
                matrix[z][counter] = item
                z+=1 
                

    #print(matrix)
    import numpy as np
    import pandas as pd
    matrix_array = np.array(matrix)
    matrix_df = pd.DataFrame(columns = ['warehouseID', 'Block', 'containerID','haulierID','remarks', 'LotID'])
    count = 0

    print(matrix_array)
    for col in range(matrix_array.shape[1]):  
        for row in range(matrix_array.shape[0]):
            #print(row)
            if matrix_array[row][col] == -1:
                continue
            matrix_df.loc[count] = ['C'] + [row] + [new_list[matrix_array[row][col]].cont_id] + [new_list[matrix_array[row][col]].company] + [' '] + [col]
            count += 1
    print(matrix_df)
    matrix_df.to_csv('out.csv',index=False)
    
    #from csv import writer
    #list_data = ['C', '0', '01/01',str(new_list[0].cont_id), new_list[0].company,' ', '0']
    
    #with open('book7.csv', 'a', newline='') as f_object:
        #writer_object = writer(f_object, delimiter=',')
        #writer_object.writerows(list_data)
        #f_object.close()
        
        
    

if __name__ == '__main__':
    main()
    
