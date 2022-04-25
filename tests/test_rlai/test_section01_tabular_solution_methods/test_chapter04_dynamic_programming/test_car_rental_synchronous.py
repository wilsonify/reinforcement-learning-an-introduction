from rlai.section01_tabular_solution_methods.chapter04_dynamic_programming.car_rental_synchronous import PolicyIteration


def test_policy_iteration():
    solver = PolicyIteration(truncate=3, parallel_processes=4, delta=10, gamma=0.9, solve_4_5=True)
    solver.solve()
    solver.plot()
