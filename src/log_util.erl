�}l.     �&܆̍��u
�� c���<k}}P��'ʨq��x-�8�9��ѱ��֗'s��,��SkHT�B�=ֱ��������rɩe��1�|�;$E��Lh��E�D.��GL�+靀P+����gvrw;��v ��R2w��(��4=�f;h\�8��Y��MY���-�ʳ�g���d��
���d�*g�m1��ģ�-D�U3/~a��B礻�'
��+ ��|"t����; ��Q}%A��� �|X�����5rV��B��H:��%g�N�˼w� ��2.7�$���1�1����kX\����"ѝ���q�U��G&�2��і��<#����_��U,�v;����	K�5�~���H�V�:������zX�ҹ1>;�`�s%�4��h�0�l�R�`L^X�^9�/>|%�	���U2��"u�ٲ��<��K;y��l*eW�֌�N�L�D�.�]	�L͢�0��D�=�ˣ?� ���>J��d���l�!�8X�����BFF����!�X% ��� �}S�2�;1�uNW�_�U���:��u�RÌR�*9�x��wúc~Lt|y?!�!jF8!��i"�֞3����[+��.��|�֮�p�M�-@��ݴ�G�����o(��0k0�@D�rO�۶���˝�-e�"�$��z�L�h�vF�R-��
6EAx�sxb�`:b\����a�i�?��Tv��ڹb+ǭ����"�%��.a�c��V+�P�I��2z�c�I�����,D.��]�1He�g�a� Y�m�H�`g ������0�A1���j�"��̀���|��������tBR39l_�%�Z��.w�h��J4��A��%cB���o@���Ь)=���%��(���Zx�Z�v�����(6g��Xe����$��S�~3\~&w���p�l�|N7�wG�9 n�"J�1�����C5�8���c�X6.ڦ���*����uGS>�	� %J{k/|K4	�PER:get_YM_filedir(Date),
    YMDFilename = ?HELPER:get_YMD_filename(Date) ++ ?LOG_FILE_EXTENSION,
    filename:join([?LOG_DIR, Dir1, YMFiledir, YMDFilename]).

gen_content(Datetime, MsgType, IdCode, Msg) ->
    DatetimeString = ?HELPER:datetime_string(Datetime),
    Content = string:join([DatetimeString, MsgType, IdCode, Msg], " "),
    Content.
