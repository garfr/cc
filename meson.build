project('ccc', 'c')

src = [
    'src/ccc.c'
]

inc = include_directories('include')

ccc = executable(
    'ccc',
    src,
    c_args : ['-Wall', '-Wextra', '-Werror', '-std=c99', '-pedantic'],
    include_directories : [inc],
)