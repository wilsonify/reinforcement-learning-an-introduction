from rlai.section01_tabular_solution_methods.chapter08_planning_and_learning_with_tabular_methods.maze import \
    example_8_4, figure_8_5, figure_8_4, figure_8_2


def test_figure_8_2():
    figure_8_2(runs=5, episodes=5)


def test_figure_8_4():
    figure_8_4(max_steps=30)


def test_figure_8_5():
    figure_8_5(max_steps=60)


def test_example_8_4():
    example_8_4(runs=2, num_of_mazes=2)
