project('cc', 'c')

src = [
    'src/cc.c'
]

inc = include_directories('include')

cc = executable(
    'cc',
    src,
    c_args : ['-Wall', '-Wextra', '-Werror', '-std=c99', '-pedantic'],
    include_directories : [inc],
)