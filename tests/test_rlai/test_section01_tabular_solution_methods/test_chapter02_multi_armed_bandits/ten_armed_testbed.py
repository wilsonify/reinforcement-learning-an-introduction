from src.rlai.section01_tabular_solution_methods.chapter02_multi_armed_bandits.ten_armed_testbed import (
    figure_2_1,
    figure_2_2,
    figure_2_3,
    figure_2_4,
    figure_2_5,
    figure_2_6
)


def test_smoke():
    print("fire?")


def test_figure_2_1():
    figure_2_1()


def test_figure_2_2():
    figure_2_2(runs=100, time=100)


def test_figure_2_3():
    figure_2_3(runs=100, time=100)


def test_figure_2_4():
    figure_2_4(runs=100, time=100)


def test_figure_2_5():
    figure_2_5(runs=100, time=100)


def test_figure_2_6():
    figure_2_6(runs=10, time=100)
