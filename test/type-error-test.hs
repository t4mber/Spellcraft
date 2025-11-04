{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- Test: This file should FAIL to compile

module TypeErrorTest where

import VHDL.Clash.Types
import VHDL.Clash.FrequencyCheck

-- This should fail: 50 * 4 = 200, not 180
badPLL :: PLL 50 4
badPLL = mkPLL @50 @4 @180 "BadPLL"
