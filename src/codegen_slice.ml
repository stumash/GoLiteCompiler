let make_slice type_str =
    let slice_str =
        "typedef struct slice_"^type_str^" {\n"^
        "  int length;\n"^
        "  int capacity;\n"^
        "  "^type_str^"* elements;\n"^
        "} slice_"^type_str^";\n"^
        "\n"^
        "void init_slice_"^type_str^"(slice_"^type_str^" *s)\n"^
        "{\n"^
        "  s->length = 0;\n"^
        "  s->capacity = 1;\n"^
        "  s->elements = malloc(sizeof("^type_str^"));\n"^
        "}\n"^
        "\n"^
        "slice_"^type_str^" *append_slice_"^type_str^"(slice_"^type_str^" *s, "^type_str^" elem)\n"^
        "{\n"^
        "  if (s->length == s->capacity)\n"^
        "  {\n"^
        "    s->elements = realloc(s->elements, (s->capacity) << 2);\n"^
        "    s->capacity <<= 2;\n"^
        "  }\n"^
        "  \n"^
        "  s->elements [s->length++] = elem;\n"^
        "  return s;\n"^
        "}\n"^
        "\n"^
        type_str^" element_get_slice_"^type_str^"(slice_"^type_str^" *s, int elem_index)\n"^
        "{\n"^
        "  if (elem_index >= s->length)\n"^
        "  {\n"^
        "    fprintf(stderr, \"slice access out of bounds\\n\");\n"^
        "    exit(1);\n"^
        "  }\n"^
        "  return s->elements[elem_index];\n"^
        "}\n"^
        "\n"^
        "int length_slice_"^type_str^"(slice_"^type_str^" *s)\n"^
        "{\n"^
        "  return s->length;\n"^
        "\n"^
        "}\n"^
        "\n"^
        "int capacity_slice_"^type_str^"(slice_"^type_str^" *s)\n"^
        "{\n"^
        "  return s->capacity;\n"^
        "}\n"^
        "\n" in
    slice_str
