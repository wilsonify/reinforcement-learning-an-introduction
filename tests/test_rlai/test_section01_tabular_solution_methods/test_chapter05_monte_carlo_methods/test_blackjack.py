from rlai.section01_tabular_solution_methods.chapter05_monte_carlo_methods.blackjack import figure_5_1, figure_5_2, \
    figure_5_3


def test_smoke():
    print("fire?")


def test_figure_5_1():
    figure_5_1(episodes1=100, episodes2=5000)


def test_figure_5_2():
    figure_5_2(episodes=5000)


def test_figure_5_3():
    figure_5_3(episodes=100, runs=10)
