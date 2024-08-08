# Spectrogram generator via: https://github.com/kylemcdonald/AudioNotebooks/blob/master/Generating%20Spectrograms.ipynb

# Copyright (c) 2016- Kyle McDonald
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import librosa
import matplotlib.pyplot as plt
import numpy as np
import cv2

class SpectrogramGenerator:
    """Create a spectrogram generator."""
    
    def __init__(self, sr,
                 n_fft=1024,
                 linear=False,
                 hop_length=None,
                 db_ref=0.01,
                 db_range=None,
                 fmin=None,
                 fmax=None,
                 cmap=None,
                 window='hann',
                 drop_nyquist=True,
                 flipud=True):
        """
        Args:
            sr (int): sample rate
            n_fft (int): size of STFT window in samples
            linear (bool): use linear instead of logarithmic frequency spacing
            hop_length (int): samples between STFT frames
            db_ref (float): `ref` parameter when converting amplitude to dB
            db_range (tuple): min and max dB for output
            fmin (float): minimum frequency to output (hertz)
            fmax (float): maximum frequency to output (hertz)
            cmap (str): matplotlib color map to use for output
            window (str): windowing function for STFT
            drop_nyquist (bool): if True, output has exactly `n_fft//2` bins
            flipud (bool): whether to flip the STFT output
        """

        self.n_fft = n_fft
        self.hop_length = hop_length
        self.db_ref = db_ref
        self.db_range = db_range
        self.cmap = cmap
        self.window = window
        self.drop_nyquist = drop_nyquist
        
        if hop_length is None:
            self.hop_length = n_fft // 4
        
        if cmap is not None:
            self.cmap = plt.get_cmap(cmap)
        
        bin_count = (n_fft // 2) + 1 
        if drop_nyquist:
            bin_count -= 1
        nyquist = sr // 2
        if fmin == None:
            min_bin = 1
        else:
            min_bin = (fmin / nyquist) * (bin_count - 1)
        if fmax == None:
            max_bin = bin_count - 1
        else:
            max_bin = (fmax / nyquist) * (bin_count - 1)
        bins = np.arange(bin_count)
        if linear:
            y_remap = (bins + min_bin) / ((bin_count - 1) / (max_bin - min_bin))
        else:
            scale_factor = bin_count / np.log10(max_bin / min_bin)
            y_remap = min_bin * 10 ** (bins / scale_factor)
        if flipud:
            y_remap = y_remap[::-1]
        self.mapy_base = y_remap.reshape(-1,1).astype(np.float32)
        
    def __call__(self, audio):
        """Generate a spectrogram for visualization.
        
        Returns:
            np.array of type np.uint8 if `db_range` or `cmap` is set, otherwise np.float32
        """
        stft = librosa.stft(audio,
                            n_fft=self.n_fft,
                            hop_length=self.hop_length,
                            window=self.window)
        if self.drop_nyquist:
            stft = stft[:-1]
        db = librosa.amplitude_to_db(np.abs(stft), ref=self.db_ref)
        mapx = np.arange(db.shape[1], dtype=np.float32).reshape(1,-1).repeat(db.shape[0], axis=0)
        mapy = self.mapy_base.repeat(db.shape[1], axis=1)
        out = cv2.remap(db, mapx, mapy, cv2.INTER_LANCZOS4)
        
        if self.db_range is not None:
            out -= self.db_range[0]
            out /= self.db_range[1] - self.db_range[0]
            
        if self.cmap is not None:
            if self.db_range is None:
                out -= out.min()
                out /= out.max()
            out = self.cmap(out)[...,:3]
            
        if self.cmap is not None or self.db_range is not None:
            out = np.clip(out * 256, 0, 255).astype(np.uint8)
            
        return out